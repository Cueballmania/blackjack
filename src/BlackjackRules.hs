module BlackjackRules where

import Deck
import Types

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

isBlackjack :: Hand -> Bool
isBlackjack h = (handValue h == 21) && (length h == 2)
