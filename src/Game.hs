{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module Game where

import Deck
import Types
import Bets
import System.Random
import Actions
import BlackjackRules
import OpeningDeal
import Insurance
import Control.Monad.Trans.State
import Control.Monad.IO.Class (liftIO)
import Control.Monad (forM, replicateM)
import Text.Read (readMaybe)

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
            put $ gs { deck = newDeck, discard = [], gen = newGen , penetration = 2 * length newDeck }
            dealCard
        (c:cs) -> do
            put $ gs { deck = cs }
            return c

playGame :: GameT IO ()
playGame = do
    g <- get
    let ps = players g
    betPlayers <- liftIO $ getBets ps
    put $ g { players = betPlayers }
    (_, g2) <- runState dealOpeningHands <$> get
    put g2
    let ps2 = players g2
    let d = dealer g2
    liftIO $ print d
    if cardValue (head (hand d)) == 1
        then do
            processInsurance
            if dealerBlackjack d
                then do
                    processDealerBlackjack
                    put $ g2 {dealer = d { hand = hand d ++ hiddenHand d, hiddenHand = [] }}
                else do
                    newPlayers <- forM ps2 $ \p -> do
                        playerTurn p
                    g3 <- get
                    put $ g3 { players = newPlayers, dealer = d { hand = hand d ++ hiddenHand d, hiddenHand = [] } }
                    dealerTurn
        else do
            newPlayers <- forM ps2 $ \p -> do
                playerTurn p
            g3 <- get
            put $ g3 { players = newPlayers, dealer = d { hand = hand d ++ hiddenHand d, hiddenHand = [] } }
            dealerTurn
    makePayouts
    cleanupHands
    playGame

makePayouts :: GameT IO ()
makePayouts = do
    gs <- get
    let ps = players gs
        d = dealer gs
    newPlayers <- forM ps $ \p -> do
        let br = bankroll p
        payouts <- liftIO $ calculatePayouts (hand d) (playedHands p)
        let winnings = sum payouts
            resetHands = [(h, 0) | (h, _) <- playedHands p]
        return $ p { bankroll = br + winnings, playedHands = resetHands }
    put $ gs { players = newPlayers }

calculatePayouts :: Hand -> [(Hand, Money)] -> IO [Money]
calculatePayouts d = mapM (calculatePayout d)

calculatePayout :: Hand -> (Hand, Money) -> IO Money
calculatePayout d (h, b)
    | isBlackjack h = if isBlackjack d
                        then do
                            putStrLn "Push"
                            return b
                        else do
                            putStrLn "Blackjack!"
                            return $ bjPay b
    | handValue h > 21 = do
                            putStrLn "Bust!"
                            return 0
    | handValue d > 21 = do
                            putStrLn "Win!"
                            return $ 2 * b
    | handValue h > handValue d = do
                            putStrLn "Win!"
                            return $ 2 * b
    | handValue h == handValue d = do
                            putStrLn "Push"
                            return b
    | otherwise = do
                            putStrLn "Loss"
                            return 0

dealerTurn :: GameT IO ()
dealerTurn = do
    gs <- get
    let d = dealer gs
    liftIO $ putStrLn $ "Dealer has " ++ show (hand d)
    if handValue (hand d) < 17
        then do
            liftIO $ putStrLn "Dealer hits"
            (newCard, newState) <- runState drawCard <$> get
            let newDealer = d { hand = hand d ++ [newCard] }
            put $ newState { dealer = newDealer }
            dealerTurn
        else if handValue (hand d) <= 21
            then do
                liftIO $ putStrLn $ "Dealer stands with " ++ show (hand d)
            else do
                liftIO $ putStrLn "Dealer busts"

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


cleanupHands :: GameT IO ()
cleanupHands = do
    gs <- get
    let ps = players gs
        d = dealer gs
        discards = discard gs
        discardPile = discards ++ hand d ++ concatMap (concatMap fst . playedHands) ps
    if length (deck gs) < penetration gs
        then do
            let (newDeck, newGen) = shuffle (discardPile ++ deck gs) (gen gs)
            put gs { dealer = d {hand = []},
               players = map (\p -> p { playedHands = [], insurance = 0 }) ps,
               deck = newDeck, gen = newGen, discard = [] }
            newPen <- cutCard
            g2 <- get
            put $ g2 { penetration = newPen }
        else put $ gs { dealer = d { hand = []},
               players = map (\p -> p { playedHands = [], insurance = 0 }) ps,
               discard = discardPile }

bjPay :: Money -> Money
bjPay b = 3*b `div` 2

promptForPlayers :: IO [String]
promptForPlayers = do
    putStrLn "Enter the number of players:"
    inputN <- getLine
    case readMaybe inputN of
        Just n | n > 0 -> do
            promptNames n
        _ -> do
            putStrLn "Invalid input. Please enter a positive integer."
            promptForPlayers

promptNames :: Int -> IO [String]
promptNames n = do
        replicateM n $ do
            putStrLn "Enter player name:"
            getLine

makeGame :: [String] -> IO Game
makeGame names = do
    let players = map (\n -> Player n [] [] 1000 0) names
    gen <- initStdGen
    let (newDeck, gen') = shuffle (genDecks 1) gen
    let cut = 4 * length newDeck `div` 9
    return $ Game newDeck [] (Dealer "Default" [] []) players cut gen'