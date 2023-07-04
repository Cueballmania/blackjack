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
        then p { hands = newHands}
        else p { hands = zipWith (++) (hands p) newHands}

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

playerTurn :: Player -> GameT IO ()
playerTurn p = do
    liftIO $ putStrLn $ "It's " ++ playerName p ++ "'s turn!"
    hs <- gets (hands p)
    bs <- gets (bet p)
    br <- gets (bankroll p)
    forM_ [1..length hs] $ \handNum -> do
        liftIO $ putStrLn $ "Processing Hand " ++ show handNum
        let hand = hs !! (handNum - 1)
        -- Blackjack logic
        if isBlackjack hand
            then do
                liftIO $ putStrLn "Blackjack!"
                -- Offer the player to double down if possible
                if canDoubleDown br bs hand
                    then do
                        liftIO $ putStrLn "You can double down. Would you like to? (y/n)"
                        choice <- liftIO getLine
                        if choice == "y"
                            then do
                                let doubledBet = 2 * (bs !! (handNum - 1))
                                updatePlayerBet p handNum doubledBet
                                dealToHand p handNum
                            else payBlackjack p handNum
                    else payBlackjack p handNum
            else do
                -- If the player can split, offer it
                if canSplit bs hand
                    then do
                        liftIO $ putStrLn "You can split. Would you like to? (y/n)"
                        choice <- liftIO getLine
                        if choice == "y"
                            then do
                                splitHand p handNum
                                playerTurn p -- Process each hand again with bets inserted
                            else processNormalHand p handNum
                    else do
                        -- If the player can double down, offer it
                        if canDoubleDown br bs hand
                            then do
                                liftIO $ putStrLn "You can double down. Would you like to? (y/n)"
                                choice <- liftIO getLine
                                if choice == "y"
                                    then do
                                        let doubledBet = 2 * (bs !! (handNum - 1))
                                        updatePlayerBet p handNum doubledBet
                                        dealToHand p handNum
                                    else processNormalHand p handNum
                            else do
                                -- Offer to hit or stand until the total is over 21
                                processNormalHand p handNum

allSame :: Eq a => [a] -> Bool
allSame xs = null xs || all (== head xs) (tail xs)

handActions :: Hand -> Bool -> Int -> [Action]
handActions h dbBet nHands = 
        | cs == True && dbBet == True && canSplit == True   = [Hit, Stand, Double, Split]
        | cs == True && dbBet == True && canSplit == False  = [Hit, Stand, Double]
        | otherwise                                         = [Hit, Stand]
        where
            cs = length h == 2
            canSplit = dbBet && allSame $ map carValue h


runHand :: Hand -> Bool -> Int -> IO ()
runHand h dbBet nHands = do
  let actions = handActions h dbBet nHands
  let prompt = buildPrompt actions
  putStr prompt
  input <- getLine
  case input of
    "H" -> if Hit `elem` actions
           then putStrLn "Player hits."
           else invalidInput
    "S" -> if Stand `elem` actions
           then putStrLn "Player stands."
           else invalidInput
    "D" -> if Double `elem` actions
           then putStrLn "Player doubles down."
           else invalidInput
    "P" -> if Split `elem` actions
           then putStrLn "Player splits."
           else invalidInput
    _ -> invalidInput
  where
    buildPrompt actions = "Enter an action (" ++ buildOptions actions ++ "): "
    buildOptions actions = buildOption 'H' "hit" actions ++ buildOption 'S' "stand" actions ++
                           buildOption 'D' "double down" actions ++ buildOption 'P' "split" actions
    buildOption key name actions
      | elem key ['H','S'] = if elem key actions then [key] ++ " for " ++ name ++ ", " else ""
      | otherwise = if elem key actions then [key] ++ " for " ++ name else ""

    invalidInput = do
      putStrLn "Invalid input."
      runHand h dbBet nHands

runHand :: Hand -> Bool -> Int -> IO ()
runHand h dbBet nHands = do
  let actions = handActions h dbBet nHands
  let prompt = "Enter an action (H for hit, S for stand, D for double down, P for split): "
  putStr prompt
  input <- getLine
  case input of
    "H" -> putStrLn "Player hits."
    "S" -> putStrLn "Player stands."
    "D" -> if Double `elem` actions
           then putStrLn "Player doubles down."
           else putStrLn "This action is not allowed."
    "P" -> if Split `elem` actions
           then putStrLn "Player splits."
           else putStrLn "This action is not allowed."
    _ -> putStrLn "Invalid input."

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