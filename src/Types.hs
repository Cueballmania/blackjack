module Types where

import Deck
import System.Random
import Control.Monad.Trans.State

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

data Player = Player { playerName :: String
                     , hands      :: [(Hand, Money)]
                     , bankroll   :: Money }
    deriving (Eq)

data Dealer = Dealer { dealerName :: String
                     , hand       :: Hand
                     , hiddenHand :: Hand }
    deriving (Eq)



instance Show Player where
    show (Player name hs br) = "Player: " ++ name ++
                               "\nHands: " ++ unwords [show h ++ "\t" | (h,b) <- hs] ++
                               "\nBets: " ++ unwords [show b ++ "\t" | (h,b) <- hs] ++
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