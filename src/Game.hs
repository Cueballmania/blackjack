module Game where

import Deck 
import Types
import Bets
import System.Random
import Control.Monad.Trans.State
import Control.Monad.IO.Class (liftIO)

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
    newHands <- replicateM numHands $ replicateM dealCard
    return $ if hands p == [[]]
        then put $ p { hands = newHands}
        else put $ p { hands = zipWith (++) (hands p) newHands}

dealToDealer :: State Game ()
dealToDealer = do
    gs <- get
    let d = dealer gs
    card <- dealCard
    return $ if length (hand d) == 1
        then d { hiddenHand = [card] }
        else d { hand = (hand d) ++ [card] }

dealOpeningHands :: State Game ()
dealOpeningHands = do
    dealToAllPlayers
    dealToDealer
    dealToAllPlayer
    dealToDealer

-- Need a function to check to offer insurance
-- Need a function that processes dealer blackjack
    -- Pushes with player blackjack and wins all other bets

processPlayer :: Player -> GameT IO ()
processPlayer p = do
     hs <- (hands p)
     bs <- (bets p)
     br <- (bankroll p)
     liftIO $ putStrLn $ show (name p) ++ " is playing " ++ show (length (hands p)) ++ " hands."
     newHands, newBets, newBankroll <- processHands hs bs br
     put $ p {hands = newHands, bets = newBets, bankroll = newBankroll}

processHands :: [Hand] -> [Money] -> Money -> GameT IO ([Hand], [Money], Money)
processHands hs bs br = do

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
                      newHands, newBets, newBr <- processHand (card1:newCard) b smallerBr False
                      newCard2 <- dealCard
                      newHands2, newBets2, newBr2 <- processHand (card2:newCard2) b newBr False
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
bjPay b = floor(1.5*b)

playGame :: GameT IO ()
playGame = do
    game <- get
    playerWBets <- liftIO $ getBets (players game)
    let dealer' = dealer game
    let (playerInitial, dealerInitial, dk, dc, g) = dealOpeningHands playerWBets dealer' (deck game, discard game, gen game)
    let game' = game { players = playerInitial, dealer = dealerInitial, deck = dk, discard = dc, gen = g }
    put game'
    -- iterate through each player and process each hand in order
    (players', dk', dc', g') <- foldM (\(ps, dkAcc, dcAcc, gAcc) player -> do
        let numPlayerHands = numHands player
        (player', newHands, dk', dc', g') <- foldM (\(p, newHandsAcc, dkAcc', dcAcc', gAcc') handIndex -> do
        (p', newHands', dk'', dc'', g'') <- processPlayerHand p handIndex (dkAcc', dcAcc', gAcc')
        let numNewHands = length newHands'
        let newHandsIndex = handIndex + numNewHands
        let p'' = if numNewHands > 0 then p' { numHands = numPlayerHands + numNewHands } else p'
        let p''' = foldr (\hand acc -> acc { hands = insertAtIndex handIndex hand (hands acc) }) p'' newHands'
        let newHandsAcc' = newHandsAcc ++ newHands'
        return (p''', newHandsAcc', dk'', dc'', g'')
    ) (player, [], dkAcc, dcAcc, gAcc) [0..(numPlayerHands - 1)]
    -- cleanup bets
    -- deal opening hands
    -- if Ace/Ten, ask for insurance bets
        -- check for dealer 21
        -- resolve bets if 21
    -- for each player
        -- process each hand in order
        -- for each hand, if it's a blackjack, tell them and pay them the payout rate
        -- if not, ask if they want to hit, stand, double (if they have enough money) or 
            -- split (if the two cards have the same numerical value and they have enough money and the number of hands is less than 4)
        -- if they split, take the second card and make a new hand after the previous, debt their account and add the bet
            -- deal the current hand another card and repeat
            -- a two-card 21 is not a blackjack and pays 1:1
        -- if they double down, double their bet, debt the account and give them one card and stop prompts for the hand
        -- if they hit, give them another card and repeat
        -- if they stand, move to the next hand
        -- if anytime the player's hand totals more than 21, immediate take the bet and move the cards to the discard pile
    -- the dealer then moves the hidden card to their hand and continues to hit until the stopping criteria
    -- all remaining bets are resolved (push = money goes back, win = double money, loss = clearing of bets)
    -- move all cards to the discard pile
    -- if the size of the deck is less than the penetration size, shuffle all cards
        -- reset the cut card
    -- if at anytime the deck runs out, shuffle the discards

playGame :: GameT IO ()
playGame = do
    game <- get
    playerWBets <- liftIO $ getBets (players game)
    let dealer' = dealer game
    let (playerInitial, dealerInitial, dk, dc, g) = dealOpeningHands playerWBets dealer' (deck game, discard game, gen game)
    let game' = game { players = playerInitial, dealer = dealerInitial, deck = dk, discard = dc, gen = g }
    put game'
    let numPlayers = length playerInitial
    let numHandsPerPlayer = maximum (map numHands playerInitial)
    let allHands = [(i, j) | i <- [0..(numPlayers - 1)], j <- [0..(numHandsPerPlayer - 1)]]
    (players', dk', dc', g') <- foldM (\(ps, dkAcc, dcAcc, gAcc) (playerIndex, handIndex) -> do
        let player = ps !! playerIndex
        let numPlayerHands = numHands player
        (player', newHands, dk', dc', g') <- foldM (\(p, newHandsAcc, dkAcc', dcAcc', gAcc') handIndex' -> do
            (p', newHands', dk'', dc'', g'') <- processPlayerHand p handIndex' (dkAcc', dcAcc', gAcc')
            let numNewHands = length newHands'
            let newHandsIndex = handIndex' + numNewHands
            let p'' = if numNewHands > 0 then p' { numHands = numPlayerHands + numNewHands } else p'
            let p''' = foldr (\hand acc -> acc { hands = insertAtIndex handIndex' hand (hands acc) }) p'' newHands'
            let newHandsAcc' = newHandsAcc ++ newHands'
            return (p''', newHandsAcc', dk'', dc'', g'')
        ) (player, [], dkAcc, dcAcc, gAcc) [handIndex]
        let ps' = insertAtIndex playerIndex player' ps
        let newHands' = filter (\hand -> fst hand == playerIndex && snd hand > handIndex) newHands
        let allHands' = [(i, j) | i <- [0..(numPlayers - 1)], j <- [0..(numHandsPerPlayer - 1)], i /= playerIndex || j >= newHandsIndex]
        let allHands'' = foldr (\hand acc -> if fst hand == playerIndex && snd hand >= newHandsIndex then acc else hand:acc) allHands' newHands
        return (ps', dk', dc', g')
    ) (playerInitial, dk, dc, g) allHands
    let dealerHand = hand (dealerInitial { hands = [playDealerHand (dk', dc', g') (hand dealerInitial)] })
    let dealerFinalScore = score dealerHand
    putStrLn $ "Dealer's final hand: " ++ show dealerHand ++ " (" ++ show dealerFinalScore ++ ")"
    mapM_ (processPlayerResult dealerFinalScore) players'
    where
        processPlayerResult :: Int -> Player -> IO ()
        processPlayerResult dealerFinalScore player = do
            let pid' = pid player
            let playerHands = hands player
            mapM_ (processHandResult pid' dealerFinalScore) playerHands

        processHandResult :: Int -> Int -> Hand -> IO ()
        processHandResult pid' dealerFinalScore playerHand = do