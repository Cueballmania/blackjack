module Main (main) where

import Deck 
import System.Random
import Control.Monad
import Control.Monad.Trans.State
import Control.Monad.Trans.Class
import Text.Read (readMaybe)
import Data.Maybe (catMaybes)
import Control.Lens

data Game = Game
    { deck        :: Deck
    , discard     :: Deck
    , dealer      :: Player
    , players     :: [Player]
    , penetration :: Int
    , gen         :: StdGen}
    deriving (Show)

type Money = Int

type Hand = [Card]

data Player = Player { playerName :: String
                     , hands      :: [Hand]
                     , bankroll   :: Money
                     , bet        :: [Money] }
            | Dealer { playerName :: String
                     , hand       :: Hand
                     , hiddenHand :: Hand }
     deriving (Eq)



instance Show Player where
    show (Player name hs br bs) = "Player: " ++ name ++
                                  "\nHands: " ++ show hs ++
                                  "\nBets: " ++ show bs ++
                                  "\nBankroll: " ++ show br
    show (Dealer name hs hh)    = if hh == []
                                    then "Dealer: " ++ name ++
                                         "\nHand: " ++ show hs
                                    else "Dealer: " ++ name ++
                                         "\nHand: " ++ hideCard
        where
            hideCard = (takeWhile (/=',') $ show (hs ++ hh)) ++ ", XXX]"

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
    return shufDeck

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

dealOpeningHands :: Monad m => GameT m ()
dealOpeningHands = do
    game <- get
    let players' = players game
    let deck' = deck game
    let dealer' = dealer game
    let (newPlayers, newDeck) = dealFirstCard ([], deck') players'
    let (dealer1, newDeck') = dealerCard newDeck dealer'
    let (startPlayers, newDeck'') = startingHands ([], newDeck') newPlayers
    let (dealer2, newDeck''') = dealerCardH newDeck'' dealer1
    let game' = game { dealer = dealer2, players = startPlayers, deck = newDeck''' }
    put game'

dealerCard :: Deck -> Player -> (Player, Deck)
dealerCard d p = (p', d')
    where
        (c, d') = drawCard d
        dealerHand = hand p
        p' = p { hand = dealerHand ++ [c] }

dealerCardH :: Deck -> Player -> (Player, Deck)
dealerCardH d p = (p', d')
    where
        (c, d') = drawCard d
        p' = p { hiddenHand = [c] }


startingHands :: ([Player], Deck) -> [Player] -> ([Player], Deck)
startingHands (processedPlayers, d) []     = (processedPlayers, d)
startingHands (processedPlayers, d) (p:ps) = startingHands (processedPlayers ++ [p'], d') ps
    where
        (p', d') = dealSecondCard p d

dealSecondCard :: Player -> Deck -> (Player, Deck)
dealSecondCard p d = (p', d')
    where
        currentHands = hands p
        (newHands, d') = drawSecond currentHands d :: ([Hand], Deck)
        p' = p { hands = newHands }

drawSecond :: [Hand] -> Deck -> ([Hand], Deck)
drawSecond hs d = (newHands, d')
    where
        (newHands, d') = drawToHand ([], d) hs

drawToHand :: ([Hand], Deck) -> [Hand]  -> ([Hand], Deck)
drawToHand (hands, d) []     = (hands, d)
drawToHand (hands, d) (h:hs) =
    let (c, newDeck) = drawCard d
        (newHand, d') = if length h == 1
                            then (h ++ [c], newDeck)
                            else (h, d)
    in drawToHand (hands ++ [newHand], d') hs

dealFirstCard :: ([Player], Deck) -> [Player] -> ([Player], Deck)
dealFirstCard (processedPlayers, d) []     = (processedPlayers, d)
dealFirstCard (processedPlayers, d) (p:ps) =  dealFirstCard (processedPlayers ++ [p'], d') ps
    where
        (p', d') = dealFirstPlayer p d

dealFirstPlayer :: Player -> Deck -> (Player, Deck)
dealFirstPlayer p d = (p', d')
    where
        numHands = length $ filter (>0) $ bet p
        (newHands, d') = drawNInitial numHands ([], d) :: ([Hand], Deck)
        p' = p { hands = newHands }

drawNInitial :: Int -> ([[Card]], Deck) -> ([[Card]], Deck)
drawNInitial 0 (c, dk) = (c, dk)
drawNInitial n (c, dk) = drawNInitial (n-1) (c ++ [[c']], dk')
    where
        (c', dk') = drawCard dk

drawCard :: Deck -> (Card, Deck)
drawCard (c:d) = (c, d)
drawCard [] = error "Cannot draw from an empty deck"


testDealer :: Player
testDealer = Dealer
    { playerName = "testDealer"
    , hand = []
    }

testPlayer1 :: Player
testPlayer1 = Player
    { playerName = "Jake"
    , hands = [[]]
    , bankroll = 100
    , bet = [1,2,3]
    }

testPlayer2 :: Player
testPlayer2 = Player
    { playerName = "Josh"
    , hands = [[]]
    , bankroll = 100
    , bet = [1,0,-1,23]
    }

testGame :: Game
testGame = Game
    { deck        = genDecks 2
    , discard     = []
    , dealer      = testDealer
    , players     = [testPlayer1, testPlayer2]
    , penetration = 66
    , gen         = mkStdGen 0}

newGame = execState dealOpeningHands testGame

main :: IO ()
main = print newGame