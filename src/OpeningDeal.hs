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

-- Deal one card to each player's bets, then the dealer, then another card to each hand
-- then the dealer's final card is hidden
dealOpeningHands :: State Game ()
dealOpeningHands = do
    gs <- get
    let ps = players gs
    let d = dealer gs
    newPlayers <- dealCardToAllPlayers ps
    newCard <- drawCard
    newPlayers2 <- dealCardToAllPlayers newPlayers
    newCard2 <- drawCard
    let newDealer2 = d { hand = [newCard], hiddenHand = [newCard2] }
    gs2 <- get
    put $ gs2 { players = newPlayers2, dealer = newDealer2 }

-- Deal one card to each player's hand using drawCard
-- Active hand is a hand with an active bet
dealCardToAllPlayers :: [Player] -> State Game [Player]
dealCardToAllPlayers ps = do
    forM ps $ \p -> do
        newHandBets <- forM (activeHands p) $ \(h, b) -> do
            newCard <- drawCard
            return (h ++ [newCard], b)
        return p { activeHands = newHandBets }
