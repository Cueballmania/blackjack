module Main (main) where

import Deck (Card, Deck, shuffle, genDecks, cardValue)
import System.Random
import Control.Monad.Trans.State
import Control.Monad.Trans.Class
import Text.Read (readMaybe)

data Game = Game
    { deck :: Deck
    , discard :: Deck
    , dealer :: Player
    , players :: [Player]
    , penetration :: Int
    , gen :: StdGen}
    deriving (Show)

type Money = Int

type Hand = [Card]

data Player = Player { playerName :: String
                     , hands :: [Hand]
                     , bankroll:: Money
                     , bet :: [Money] }
            | Dealer { playerName :: String
                     , hand :: Hand }
     deriving (Eq)

instance Show Player where
    show (Player name hs br bs) = "Player: " ++ name ++
                                  "\nHands: " ++ show hs ++
                                  "\nBets: " ++ show bs ++
                                  "\nBankroll: " ++ show br
    show (Dealer name hs)       = "Dealer: " ++ name ++
                                  "\nHand: " ++ show hs

type GameT m = StateT Game m

printTableValue :: [Card] -> IO ()
printTableValue cs = putStrLn $ "The value of cards is " ++ show vals
    where vals = sum [cardValue c | c <- cs]

shuffleDeck :: Monad m => GameT m Deck
shuffleDeck = do
    game <- get
    let (shufDeck, gen') = shuffle (deck game) (gen game)
    let game' = game {deck = shufDeck, gen = gen'}
    put game'

cutCard :: Monad m => GameT m Int
cutCard :: do
    game <- get
    let dkSize = length (deck game)
    let (index, gen') = randomR (0.35 * fromIntegral dkSize
                                  ,0.45 * fromIntegral dkSize) (gen game)
    let pen = ceiling index
    let game' = game { penetration = pen, gen = gen')
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

drawCard :: Monad m => GameT m (Maybe [Card])
drawCard = do
    g <- get
    let deck' = deck g
    if n > length deck'
        then return Nothing
        else do
            let (drawnCard, remainingDeck) = (head deck', tail deck')
            put $ g { deck = remainingDeck }
            return $ Just drawnCard

drawN :: Monad m => Int -> GameT m (Maybe [Card])
drawN n = do
    g <- get
    let deck' = deck g
    let tabled = table g
    let discarded = discard g
    if n > length deck'
        then return Nothing
        else do
            let (drawnCards, remainingDeck) = splitAt n deck'
            put $ g { deck = remainingDeck, table = drawnCards, discard = tabled ++ discarded}
            return $ Just drawnCards

clearTable :: Monad m => GameT m ()
clearTable = do
    g <- get
    let tabled = table g
    let discarded = discard g
    put $ g {table = [], discard = tabled ++ discarded}

gameLoop :: GameT IO ()
gameLoop = do
    g <- get
    lift $ putStrLn "How many cards would you like to draw?"
    drawStr <- lift getLine
    case readMaybe drawStr of
        Nothing -> 
            if drawStr == "quit" then lift $ return ()
            else do
                lift $ putStrLn "Invalid input. Please enter a number."
                gameLoop
        Just draw -> do
            cards <- drawN draw
            case cards of
                Nothing -> do
                    let remaining = length $ deck g
                    lift $ putStrLn $ "Not enough cards left to draw " ++ 
                            show draw ++ ". Only " ++ show remaining ++ " cards remaining."
                Just xs -> do
                    lift $ putStrLn $ "Table: " ++ show xs
                    lift $ printTableValue xs
                    gameLoop

main :: IO ()
main = do
    putStrLn "Enter a seed value: "
    seedStr <- getLine
    let seed = read seedStr :: Int
    let initGen = mkStdGen seed
    let initialGame = Game {deck = genDecks 6, discard = [], table = [], gen = initGen}
    _ <- execStateT (shuffleDeck >> gameLoop) initialGame
    putStrLn "Thanks for playing"
