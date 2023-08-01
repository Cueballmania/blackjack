module Game (
    playGame,
    makeGame,
    promptForPlayers
) where

import Deck ( genDecks, shuffle, cardValue )
import Types
    ( GameT,
      Dealer(Dealer, hiddenHand, dealerName, hand),
      Player(Player, activeHands, playedHands, insurance, bankroll,
             playerName),
      Hand,
      Money,
      Game(..) )
import Bets ( getBets )
import System.Random ( initStdGen, Random(randomR) )
import Actions ( playerTurn, drawCard )
import BlackjackRules ( handValue, dealerBlackjack, isBlackjack, bjPay )
import OpeningDeal ( dealOpeningHands )
import Insurance ( processInsurance )
import Control.Monad.Trans.State ( get, put, runState)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (forM, filterM, replicateM)
import Text.Read (readMaybe)

-- Calculates a position for the cut card
-- If the deck is ever smaller than the the cut card position, the deck is 
-- shuffled at the end of the hand
cutCard :: Monad m => GameT m Int
cutCard = do
    gs <- get
    let dkSize = length (deck gs)
    let (index, gen') = randomR ((0.35 * fromIntegral dkSize) :: Double
                                ,(0.45 * fromIntegral dkSize) :: Double) (gen gs)
    let pen = ceiling index
    let gs' = gs { penetration = pen, gen = gen' }
    put gs'
    return pen

-- Main loop for the game
-- Continue the game until all players are broke
playGame :: GameT IO ()
playGame = do
    gs <- get
    let ps = players gs
    betPlayers <- liftIO $ getBets ps
    put $ gs { players = betPlayers }
    (_, gs2) <- runState dealOpeningHands <$> get
    put gs2
    let ps2 = players gs2
    let d = dealer gs2
    _ <- liftIO $ putStrLn ""
    liftIO $ print d
    if cardValue (head (hand d)) == 1
        then do
            processInsurance
            if dealerBlackjack d
                then do
                    processDealerBlackjack
                    gs3 <- get
                    put $ gs3 {dealer = d { hand = hand d ++ hiddenHand d, hiddenHand = [] }}
                else do
                    newPlayers <- forM ps2 $ \p -> do
                        _ <- liftIO $ putStrLn ""
                        playerTurn p
                    gs3 <- get
                    put $ gs3 { players = newPlayers, dealer = d { hand = hand d ++ hiddenHand d, hiddenHand = [] } }
                    _ <- liftIO $ putStrLn ""
                    dealerTurn
        else do
            newPlayers <- forM ps2 $ \p -> do
                _ <- liftIO $ putStrLn ""
                playerTurn p
            gs3 <- get
            put $ gs3 { players = newPlayers, dealer = d { hand = hand d ++ hiddenHand d, hiddenHand = [] } }
            _ <- liftIO $ putStrLn ""
            dealerTurn
    makePayouts
    cleanupHands
    cleanupPlayers
    gs4 <- get
    if not (null (players gs4))
        then do
            _ <- liftIO $ putStrLn "Next hand! \n\n"
            playGame
        else do
            _ <- liftIO $ putStrLn "Game Over"
            return ()

-- Calculates payouts to each player and updates their bankroll
makePayouts :: GameT IO ()
makePayouts = do
    gs <- get
    let ps = players gs
        d = dealer gs
    newPlayers <- forM ps $ \p -> do
        let br = bankroll p
        _ <- liftIO $ putStrLn $ "\nPlayer " ++ playerName p ++ "'s hands:"
        payouts <- liftIO $ calculatePayouts (hand d) (playedHands p)
        let winnings = sum payouts
            resetHands = [(h, 0) | (h, _) <- playedHands p]
        _ <- liftIO $ putStrLn $ "Player " ++ playerName p ++ " now has " ++ show (br + winnings)
        return $ p { bankroll = br + winnings, playedHands = resetHands }
    put $ gs { players = newPlayers }

calculatePayouts :: Hand -> [(Hand, Money)] -> IO [Money]
calculatePayouts d = mapM (calculatePayout d)

-- Calculates and outputs the result of each hand
calculatePayout :: Hand -> (Hand, Money) -> IO Money
calculatePayout d (h, b)
    | isBlackjack h = if isBlackjack d
                        then do
                            putStrLn $ "Hand: " ++ show h ++ " (" ++ show (handValue h) ++ ") is a Blackjack but Pushes"
                            return b
                        else do
                            putStrLn $ "Hand: " ++ show h ++ "is a Blackjack! Payout: " ++  show (bjPay b)
                            return $ bjPay b
    | handValue h > 21 = do
                            putStrLn $ "Hand: " ++ show h ++ " (" ++ show (handValue h) ++") Busts!"
                            return 0
    | handValue d > 21 = do
                            putStrLn $ "Hand: " ++ show h ++ " (" ++ show (handValue h) ++") Wins! Payout: " ++ show b
                            return $ 2 * b
    | handValue h > handValue d = do
                            putStrLn $ "Hand: " ++ show h ++ " (" ++ show (handValue h) ++") Wins! Payout: " ++ show b
                            return $ 2 * b
    | handValue h == handValue d = do
                            putStrLn $ "Hand: " ++ show h ++ " (" ++ show (handValue h) ++ ") Pushes"
                            return b
    | otherwise = do
                            putStrLn $ "Hand: " ++ show h ++ " (" ++ show (handValue h) ++") Loses"
                            return 0

-- Processes the dealer's turn of hitting all 16s
dealerTurn :: GameT IO ()
dealerTurn = do
    gs <- get
    let d = dealer gs
    liftIO $ putStrLn $ "Dealer has " ++ show (hand d) ++ " for a value of " ++ show (handValue (hand d))
    if handValue (hand d) < 17
        then do
            liftIO $ putStrLn "Dealer hits"
            (newCard, newState) <- runState drawCard <$> get
            let newDealer = d { hand = hand d ++ [newCard] }
            put $ newState { dealer = newDealer }
            dealerTurn
        else if handValue (hand d) <= 21
            then do
                liftIO $ putStrLn $ "Dealer stands with " ++ show (hand d) ++ " for a value of " ++ show (handValue (hand d))
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

-- Move all cards to the discard pile and shuffle if penetration is reached
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

-- If player is out of money, remove them from the game
cleanupPlayers :: GameT IO ()
cleanupPlayers  = do
    gs <- get
    let ps = players gs
    newPlayers <- filterM (\p -> do
        let br = bankroll p
        if br <= 0
            then do
                liftIO $ putStrLn $ "Player " ++ playerName p ++ " is out of money."
                return False
            else do
                return True) ps
    put $ gs { players = newPlayers }

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

-- Initialize a new game with a random seed and 6 decks
makeGame :: [String] -> IO Game
makeGame names = do
    let initPlayers = map (\n -> Player n [] [] 1000 0) names
    initGen <- initStdGen
    let (newDeck, gen') = shuffle (genDecks 6) initGen
    let cut = 4 * length newDeck `div` 9
    return $ Game {deck = newDeck,
                   discard = [],
                   dealer = Dealer { dealerName = "Default",
                                     hand = [],
                                     hiddenHand = [] }, 
                   players = initPlayers,
                   penetration = cut,
                   gen = gen' }