module Types (
    Game (..),
    GameT,
    Money,
    Hand,
    Player (..),
    Dealer (..)
) where

import Deck ( Deck, Card )
import System.Random ( StdGen )
import Control.Monad.Trans.State ( StateT )

data Game = Game
    { deck        :: Deck
    , discard     :: Deck
    , dealer      :: Dealer
    , players     :: [Player]
    , penetration :: Int
    , gen         :: StdGen}
    deriving (Show)

type Money = Int

type Hand = [Card]

data Player = Player { playerName  :: String
                     , activeHands :: [(Hand, Money)]
                     , playedHands :: [(Hand, Money)]
                     , bankroll    :: Money 
                     , insurance   :: Money }
    deriving (Eq)

data Dealer = Dealer { dealerName :: String
                     , hand       :: Hand
                     , hiddenHand :: Hand }
    deriving (Eq)



instance Show Player where
    show (Player name ah ph br _) = "Player: " ++ name ++
                                  "\nHands: " ++ unwords [show h ++ "\t" | (h,_) <- ah ++ ph] ++
                                  "\nBets: " ++ unwords [show b ++ "\t" | (_,b) <- ah ++ ph] ++
                                  "\nBankroll: " ++ show br
                                  
instance Show Dealer where
    show (Dealer name hs hh)    = if null hh
                                    then "Dealer: " ++ name ++
                                         "\nHand: " ++ show hs
                                    else "Dealer: " ++ name ++
                                         "\nHand: " ++ openingHand
        where
            openingHand = show hs ++ ", XXX"

type GameT m = StateT Game m