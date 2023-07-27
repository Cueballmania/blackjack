module OpeningDeal where

import Types
import Deck
import Actions  (drawCard)
import Control.Monad.Trans.State
import Control.Monad (forM_)

dealOpeningHands :: State Game ()
dealOpeningHands = do
    g <- get
    let ps = players g
    let d = dealer g
    -- for each player, access the active hands
    -- for each active hand, draw a card
    -- return each player with a card added to each active hand keeping the original bet
 
    


