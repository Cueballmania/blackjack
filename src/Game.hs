module Game where

import Deck
import Types
import Bets
import System.Random
import Actions
import BlackjackRules
import OpeningDeal
import Control.Monad.Trans.State
import Control.Monad.IO.Class (liftIO)
import Control.Monad (replicateM, forM)

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

playGame :: GameT IO ()
playGame = do
    g <- get
    let ps = players g
    betPlayers <- getBets ps
    put $ g { players = betPlayers }
    dealOpeningHands
    let d = dealer g
    if cardValue . head . hiddenHand d == 1
        then do
            processInsurance
            if dealerBlackjack d
                then do
                    processDealerBlackjack
                    put $ g {dealer = d { hand = hand d ++ hiddenHand d, hiddenHand = [] }}
                else do
                    playerTurn
                    put $ g { dealer = d { hand = hand d ++ hiddenHand d, hiddenHand = [] } }
                    dealerTurn
        else do
            playerTurn
            put $ g { dealer = d { hand = hand d ++ hiddenHand d, hiddenHand = [] } }
            dealerTurn
    makePayouts
    cleanupHands
    playGame

makePayouts :: GameT IO ()
makePayouts = do
    gs <- get
    let ps = players gs
        d = dealer gs
    newPlayers <- forM ps $ \p ->
        let br = bankroll p
        payouts <- calculatePayouts d (playedHands p)
        let winnings = sum payouts
            resetHands = [(h, 0) | (h, _) <- playedHands p]
        in return $ p { bankroll = br + winnings, playedHands = resetHands }
    put $ gs { players = newPlayers }

calculatePayouts :: Hand -> [(Hand, Money)] -> IO [Money]
calculatePayouts d = mapM (calculatePayout d)

calculatePayout :: Hand -> (Hand, Money) -> IO Money
calculatePayout d (h, b)
    | isBlackjack h = if isBlackjack d
                        then do
                            liftIO $ putStrLn "Push"
                            b
                        else do
                            liftIO $ putStrLn "Blackjack!"
                            bjPay b
    | handValue h > 21 = do
                            liftIO $ putStrLn "Bust!"
                            0
    | handValue d > 21 = do
                            liftIO $ putStrLn "Win!"
                            2 * b
    | handValue h > handValue d = do
                            liftIO $ putStrLn "Win!"
                            2 * b
    | handValue h == handValue d = do
                            liftIO $ putStrLn "Push"
                            b
    | otherwise = do
                            liftIO $ putStrLn "Loss"
                            0

dealerTurn :: GameT IO ()
dealerTurn = do
    gs <- get
    let d = dealer gs
    liftIO $ putStrLn $ "Dealer has " ++ show (hand d)
    if handValue (hand d) < 17
        then do
            liftIO $ putStrLn "Dealer hits"
            newCard <- dealCard
            let newDealer = d { hand = hand d ++ [newCard] }
            put $ gs { dealer = newDealer }
            dealerTurn
        else do
            liftIO $ putStrLn $ "Dealer stands with " ++ show (hand d)

-- If dealer has blackjack, make all player hands played
processDealerBlackjack :: GameT IO ()
processDealerBlackjack = do
    gs <- get
    let ps = players gs
    liftIO $ putStrLn "Dealer has blackjack."
    newPlayers <- forM ps $ \p -> do
        let ah = activeHands p
        return $ p { playedHands = ah, activeHands = [] }
    put $ gs { players = newPlayers }

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
        discardPile = discard gs ++ hand d ++ concatMap (concatMap fst . playedHands) ps
    put $ gs { dealer = d { hand = []},
               players = map (\p -> p { playedHands = [], insurance = 0 }) ps,
               discard = discardPile ++ discard gs }

-- processPlayer :: Player -> GameT IO Player
-- processPlayer p = do
--     let hs = hands p
--         bs = bet p
--         br = bankroll p
--     liftIO $ putStrLn $ show (playerName p) ++ " is playing " ++ show (length (hands p)) ++ " hands."
--     (newHands, newBets, newBankroll) <- processHands hs bs br
--     return p {hands = newHands, bet = newBets, bankroll = newBankroll}

-- processHands :: [Hand] -> [Money] -> Money -> GameT IO ([Hand], [Money], Money)
-- processHands [] _ br = return ([], [], br)
-- processHands (h:hs) (b:bs) br = do
--     (newHand, newBet, newBankroll) <- processHand h b br True
--     (remainingHands, remainingBets, remainingBankroll) <- processHands hs bs newBankroll
--     return (newHand ++ remainingHands, newBet ++ remainingBets, remainingBankroll)
-- processHands _ _ _ = return ([], [], 0)

-- processHand :: Hand -> Money -> Money -> Bool -> GameT IO ([Hand], [Money], Money)
-- processHand h b br canBJ = do
--     if isBlackjack h && canBJ
--         then do
--             liftIO $ putStrLn "Blackjack!"
--         else do
--             liftIO $ putStrLn "Not a blackjack."
--     if canSplit h b br
--         then do
--             liftIO $ putStrLn "You can split this hand."
--             liftIO $ putStrLn "Do you want to split? (y/n)"
--             choice <- liftIO getLine
--             if choice == "y"
--                 then do
--                     let (card1 : card2 : _) = h
--                         smallerBr = br - b
--                     gs <- get
--                     let (newCard, newState) = runState dealCard gs
--                     put newState
--                     (newHands, newBets, newBr) <- processHand [card1, newCard] b smallerBr False
--                     gs2 <- get
--                     let (newCard2, newState2) = runState dealCard gs2
--                     put newState2
--                     (newHands2, newBets2, newBr2) <- processHand [card2, newCard2] b newBr False
--                     return (newHands ++ newHands2, newBets ++ newBets2, newBr2)
--                 else do
--                     canDoubleHand h b br
--         else do
--             canDoubleHand h b br


-- canSplit :: Hand -> Money -> Money -> Bool
-- canSplit h b br =
--     length h == 2 && cardValue (head h) == cardValue (last h) && b <= br

-- canDoubleHand :: Hand -> Money -> Money -> GameT IO ([Hand], [Money], Money)
-- canDoubleHand h b br = do
--     if canDouble h b br
--         then do
--             liftIO $ putStrLn "You can double this hand."
--             liftIO $ putStrLn "Do you want to double? (y/n)"
--             choice <- liftIO getLine
--             if choice == "y"
--                 then do
--                     let smallerBr = br - b
--                     gs <- get
--                     let (newCard, newState) = runState dealCard gs
--                     put newState
--                     return ([h ++ [newCard]], [b], smallerBr)
--                 else do
--                     newHand <- processHitStand h
--                     return ([newHand], [b], br)
--         else do
--             newHand <- processHitStand h
--             return ([newHand], [b], br)

-- canDouble :: Hand -> Money -> Money -> Bool
-- canDouble h b br =
--     length h == 2 && handSum h `elem` [10, 11] && b <= br

-- processHitStand :: Hand -> GameT IO Hand
-- processHitStand h = do
--     if handValue h > 21
--         then do
--             liftIO $ putStrLn "You busted"
--             return h
--         else do
--             liftIO $ putStrLn "Would you like to hit? (y/n)"
--             choice <- liftIO getLine
--             if choice == "y"
--                 then do
--                     gs <- get
--                     let (newCard, newState) = runState dealCard gs
--                     put newState
--                     processHitStand (h ++ [newCard])
--                 else do
--                     liftIO $ putStrLn "You stand"
--                     return h
-- dealToAllPlayers :: State Game ()
-- dealToAllPlayers = do
--     gs <- get
--     let ps = players gs
--     ps' <- mapM dealToPlayer ps
--     put $ gs { players = ps' }

-- dealToPlayer :: Player -> State Game Player
-- dealToPlayer p = do
--     let numHands = length $ bet p
--     newHands <- replicateM numHands $ replicateM 1 dealCard
--     return $ if hands p == [[]]
--         then  p { hands = newHands}
--         else  p { hands = zipWith (++) (hands p) newHands}

-- dealToDealer :: State Game ()
-- dealToDealer = do
--     gs <- get
--     let d = dealer gs
--     card <- dealCard
--     if length (hand d) == 1
--         then put $ gs {dealer = d { hiddenHand = [card] }}
--         else put $ gs {dealer = d { hand = hand d ++ [card] }}
--     return ()

-- dealOpeningHands :: State Game ()
-- dealOpeningHands = do
--     dealToAllPlayers
--     dealToDealer
--     dealToAllPlayers
--     dealToDealer

-- processInsurance :: GameT IO ()
-- processInsurance = do
--     gs <- get
--     let ps = players gs
--         d = dealer gs
--     if cardValue (head (hand d)) == 1
--         then do
--             -- Offer insurance bet to each player
--             -- Collect the players who chose to bet insurance
--             -- Resolve insurance bets
--             -- Resolve dealer blackjack
--             newPlayers <- forM ps $ \p -> do
--                 liftIO $ putStrLn $ "Player " ++ show (playerName p) ++ ", would you like to bet insurance? (y/n)"
--                 choice <- liftIO getLine
--                 if choice == "y"
--                     then do
--                         let maxBet = max (sum (bet p) `div` 2) (bankroll p)
--                         liftIO $ putStrLn $ "You can bet up to " ++ show maxBet ++ ". How much would you like to bet?"
--                         bet <- liftIO getLine
--                         if read bet > maxBet
--                             then do
--                                 let newBankroll = bankroll p - maxBet
--                                 let newInsurance = maxBet
--                                 return $ p { bankroll = newBankroll, insurance = newInsurance }
--                             else do
--                                 let newBankroll = bankroll p - read bet
--                                 let newInsurance = read bet
--                                 return $ p { bankroll = newBankroll, insurance = newInsurance }
--                     else do
--                         return p
--             if cardValue (head (hiddenHand d)) == 10
--                 then do
--                     liftIO $ putStrLn "Dealer has blackjack. Insurance bets win."
--                     let newPlayers' = map (\p -> p { bankroll = bankroll p+ 3 * insurance p, insurance = 0 }) newPlayers
--                     put $ gs { players = newPlayers' }
--                 else do
--                     liftIO $ putStrLn "Dealer does not have blackjack. Insurance bets lose."
--                     let newPlayers' = map (\p -> p { insurance = 0 }) newPlayers
--                     put $ gs { players = newPlayers'}
--         else do
--             liftIO $ putStrLn "Dealer does not have an Ace. No insurance bets."
--             return ()

allSame :: Eq a => [a] -> Bool
allSame xs = null xs || all (== head xs) (tail xs)

bjPay :: Money -> Money
bjPay b = 3*b `div` 2

