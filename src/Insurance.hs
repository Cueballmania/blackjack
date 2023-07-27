module Insurance where

import Types (Dealer(..))
import Deck (cardValue)

dealerBlackjack :: Dealer -> Bool
dealerBlackjack d = length h == 2 && handValue == 21
    where
        h = hand d ++ hiddenHand d
        handValue = sum $ map cardValue h