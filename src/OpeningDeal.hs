module OpeningDeal (
    dealOpeningHands
) where

import Types
    ( Dealer(hiddenHand, hand),
      Player(activeHands),
      Game(dealer, players) )
import Actions  (drawCard)
import Control.Monad.Trans.State ( get, put, State )
import Control.Monad (forM)

dealOpeningHands :: State Game ()
dealOpeningHands = do
    g <- get
    let ps = players g
    let d = dealer g
    newPlayers <- dealCardToAllPlayers ps
    newCard <- drawCard
    newPlayers2 <- dealCardToAllPlayers newPlayers
    newCard2 <- drawCard
    let newDealer2 = d { hand = [newCard], hiddenHand = [newCard2] }
    g2 <- get
    put $ g2 { players = newPlayers2, dealer = newDealer2 }

dealCardToAllPlayers :: [Player] -> State Game [Player]
dealCardToAllPlayers ps = do
    forM ps $ \p -> do
        newHandBets <- forM (activeHands p) $ \(h, b) -> do
            newCard <- drawCard
            return (h ++ [newCard], b)
        return p { activeHands = newHandBets }
