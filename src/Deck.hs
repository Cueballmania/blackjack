module Deck where

import System.Random (StdGen, randomR)

data Face = Ace | Two | Three | Four | Five | Six |
            Seven | Eight | Nine | Ten | Jack | Queen | King
            deriving (Show, Eq, Enum)

data Suit = Clubs | Diamonds | Hearts | Spades
            deriving (Show, Eq, Enum)

data Card = Card Face Suit
            deriving (Show, Eq)

instance Ord Card where
    compare (Card f1 _) (Card f2 _) = compare (cardValue (Card f1 Clubs)) (cardValue (Card f2 Clubs))


type Deck = [Card]

fullDeck :: [Card]
fullDeck = [Card face suit | suit <- [Clubs .. Spades], face <- [Ace .. King]]

genDecks :: Int -> [Card]
genDecks n = concat $ replicate n fullDeck

shuffle :: Deck -> StdGen -> (Deck, StdGen)
shuffle [] g = ([], g)
shuffle dk g =
    let (index, newGen) = randomR (0, length dk - 1) g
        card = dk !! index
        rest = take index dk ++ drop (index + 1) dk
    in (card : fst (shuffle rest newGen), newGen)

draw :: Deck -> (Card, Deck)
draw dk = (head dk, tail dk)

-- Make Ace 1 and use logic in rules to determine if it should be 1 or 11
cardValue :: Card -> Int
cardValue (Card face _) =
    case face of
        Ace   -> 1
        Two   -> 2
        Three -> 3
        Four  -> 4
        Five  -> 5
        Six   -> 6
        Seven -> 7
        Eight -> 8
        Nine  -> 9
        _     -> 10
