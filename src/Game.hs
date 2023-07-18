module Game where

import Deck
import Types
import Bets
import System.Random
import Control.Monad.Trans.State
import Control.Monad.IO.Class (liftIO)
import Control.Monad (replicateM, forM)
import Control.Exception (bracket)

printTableValue :: [Card] -> IO ()
printTableValue cs = putStrLn $ "The value of cards is " ++ show vals
    where vals = sum [cardValue c | c <- cs]

cutCard :: Monad m => GameT m Int
cutCard = do
    game <- get
    let dkSize = length (deck game)
    let (index, gen') = randomR ((0.35 * fromIntegral dkSize) :: Double
                                ,(0.45 * fromIntegral dkSize) :: Double) (gen game)
    let pen = ceiling index
    let game' = game { penetration = pen, gen = gen' }
    put game'
    return pen

handSum :: Hand -> Int
handSum [] = 0
handSum hs = sum $ map cardValue hs

hasAce :: Hand -> Bool
hasAce = any (\(Card f _) -> f == Ace)

isSoft :: Hand -> Bool
isSoft hs =  (handSum hs + 10 <= 21) && hasAce hs

handValue :: Hand -> Int
handValue hs = if isSoft hs
                    then handSum hs + 10
                    else handSum hs

isBlackjack :: Hand -> Bool
isBlackjack h = (handValue h == 21) && (length h == 2)

dealerBlackjack :: Dealer -> Bool
dealerBlackjack d = isBlackjack $ hand d ++ hiddenHand d

dealCard :: State Game Card
dealCard = do
    gs <- get
    case deck gs of
        [] -> do
            let (newDeck, newGen) = shuffle (discard gs) (gen gs)
            put $ gs { deck = newDeck, discard = [], gen = newGen }
            dealCard
        (c:cs) -> do
            put $ gs { deck = cs }
            return c

dealToAllPlayers :: State Game ()
dealToAllPlayers = do
    gs <- get
    let ps = players gs
    ps' <- mapM dealToPlayer ps
    put $ gs { players = ps' }

dealToPlayer :: Player -> State Game Player
dealToPlayer p = do
    let numHands = length $ bet p
    newHands <- replicateM numHands $ replicateM 1 dealCard
    return $ if hands p == [[]]
        then  p { hands = newHands}
        else  p { hands = zipWith (++) (hands p) newHands}

dealToDealer :: State Game ()
dealToDealer = do
    gs <- get
    let d = dealer gs
    card <- dealCard
    if length (hand d) == 1
        then put $ gs {dealer = d { hiddenHand = [card] }}
        else put $ gs {dealer = d { hand = hand d ++ [card] }}
    return ()

dealOpeningHands :: State Game ()
dealOpeningHands = do
    dealToAllPlayers
    dealToDealer
    dealToAllPlayers
    dealToDealer

processInsurance :: GameT IO ()
processInsurance = do
    gs <- get
    let ps = players gs
        d = dealer gs
    if cardValue (head (hand d)) == 1
        then do
            -- Offer insurance bet to each player
            -- Collect the players who chose to bet insurance
            -- Resolve insurance bets
            -- Resolve dealer blackjack
            newPlayers <- forM ps $ \p -> do
                liftIO $ putStrLn $ "Player " ++ show (playerName p) ++ ", would you like to bet insurance? (y/n)"
                choice <- liftIO getLine
                if choice == "y"
                    then do
                        let maxBet = max (sum (bet p) * 0.5) (bankroll p)
                        liftIO $ putStrLn $ "You can bet up to " ++ show maxBet ++ ". How much would you like to bet?"
                        bet <- liftIO getLine
                        if read bet > maxBet
                            then do
                                let newBankroll = bankroll p - maxBet
                                let newInsurance = maxBet
                                return $ p { bankroll = newBankroll, insurance = newInsurance }
                            else do
                                let newBankroll = bankroll p - read bet
                                let newInsurance = read bet
                                return $ p { bankroll = newBankroll, insurance = newInsurance }
                    else do
                        return p
            if cardValue (head (hiddenHand d)) == 10
                then do
                    liftIO $ putStrLn "Dealer has blackjack. Insurance bets win."
                    let newPlayers' = map (\p -> p { bankroll = bankroll p+ 3 * insurance p, insurance = 0 }) newPlayers
                    put $ gs { players = newPlayers' }
                else do
                    liftIO $ putStrLn "Dealer does not have blackjack. Insurance bets lose."
                    let newPlayers' = map (\p -> p { insurance = 0 }) newPlayers
                    put $ gs { players = newPlayers'}
        else do
            liftIO $ putStrLn "Dealer does not have an Ace. No insurance bets."
            return ()

processDealerBlackjack :: GameT IO ()
processDealerBlackjack = do
    gs <- get
    let ps = players gs
        d = dealer gs
    if dealerBlackjack d
        then do
            liftIO $ putStrLn "Dealer has blackjack."
            -- Dealer has blackjack so for each player, every hand is checked for blackjack
            -- If the hand is not a blackjack, the bet is lost and the cards are taken
            -- If the hand is blackjack then the bet is returned to the player
            newPlayers <- forM ps $ \p -> do
                let hs = hands p
                    bs = bet p
                    br = bankroll p
                newBankroll <- checkForBlackjack hs bs br
                return $ p { bet = replicate (length bs) 0, bankroll = newBankroll }
            put $ gs { players = newPlayers }
            cleanupHands
        else do
            return ()

-- Function checks each hand and bet combination
-- If the hand is blackjack, the bet is added to the bankroll and output is "Push"
-- If the hand is not blackjack, the bet is lost and output is "Loss"
-- Returns the updated bankroll
checkForBlackjack :: [Hand] -> [Money] -> Money -> GameT IO Money
checkForBlackjack (h:hs) (b:bs) br = do
    if isBlackjack h
        then do
            liftIO $ putStrLn "Push"
            let newBankroll = br + b
            checkForBlackjack hs bs newBankroll
        else do
            liftIO $ putStrLn "Loss"
            let newBankroll = br
            checkForBlackjack hs bs newBankroll
checkForBlackjack _ _ br = return br

cleanupHands :: GameT IO ()
cleanupHands = do
    gs <- get
    let ps = players gs
        d = dealer gs
        dh = hiddenHand d
        discardPile = discard gs ++ hand d ++ hiddenHand d + concatMap (concat . hands) ps
    put $ gs { dealer = d { hand = [], hiddenHand = [] },
               players = map (\p -> p { hands = [[]], bet = [], insurance = 0 }) ps,
               discard = discardPile ++ discard gs }

processPlayer :: Player -> GameT IO ()
processPlayer p = do
    hs <- hands p
    bs <- bets p
    br <- bankroll p
    liftIO $ putStrLn $ show (playerName p) ++ " is playing " ++ show (length (hands p)) ++ " hands."
    (newHands, newBets, newBankroll) <- processHands hs bs br
    put $ p {hands = newHands, bet = newBets, bankroll = newBankroll}
    return ()

processHands :: [Hand] -> [Money] -> Money -> GameT IO ([Hand], [Money], Money)
processHands [] bs br = return ([], bs, br)
processHands (h:hs) (b:bs) br = do
    (newHand, newBet, newBankroll) <- processHand h b br True
    (remainingHands, remainingBets, remainingBankroll) <- processHands hs bs newBankroll
    return (newHand : remainingHands, newBet : remainingBets, remainingBankroll)

processHand :: Hand -> Money -> Money -> Bool -> GameT IO ([Hand], [Money], Money)
processHand h b br canBJ = do
    if (isBlackjack h && canBJ)
        then do
            liftIO $ putStrLn "Blackjack!"
        else do
            liftIO $ putStrLn "Not a blackjack."
    if (canSplit h b br)
        then do
            liftIO $ putStrLn "You can split this hand."
            liftIO $ putStrLn "Do you want to split? (y/n)"
            choice <- liftIO getLine
            if choice == "y"
                then do
                    let (card1:card2) = h
                        smallerBr = br - bet
                    newCard <- dealCard
                    (newHands, newBets, newBr) <- processHand (card1:newCard) b smallerBr False
                    newCard2 <- dealCard
                    (newHands2, newBets2, newBr2) <- processHand (card2:newCard2) b newBr False
                    return (newHands:newHands2) (newBets:newBets2) newBr2
                else do
                    canDoubleHand h b br
        else do
            canDoubleHand h b br

canDoubleHand :: Hand -> Money -> Money -> GameT IO ([Hand], [Money], Money)
canDoubleHand h b br = do
    if (canDouble h b br)
        then do
            liftIO $ putStrLn "You can double this hand."
            liftIO $ putStrLn "Do you want to double? (y/n)"
            choice <- liftIO getLine
            if choice == "y"
                then do
                    let smallerBr = br - bet
                    newCard <- dealCard
                    return [h:newCard] [b] smallerBr
                else do
                    newHand <- processHitStand h
                    return [newHand] [b] br
        else do
            newHand <- processHitStand h
            return [newHand] [b] br

processHitStand :: Hand -> GameT IO Hand
processHitStand h = do
    if (handValue > 21)
        then do
            liftIO $ putStrLn "You busted"
            return h
        else do
            liftIO $ putStrLn "Would you like to hit?"
            if choice == "y"
                then do
                    card <- drawCard
                    processHitStand (h:card)
                else do
                    liftIO $ putStrLn "You stand"
                    return h

allSame :: Eq a => [a] -> Bool
allSame xs = null xs || all (== head xs) (tail xs)

bjPay :: Money -> Money
bjPay b = floor (1.5*b)

