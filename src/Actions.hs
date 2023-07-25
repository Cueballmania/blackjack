module Actions where

import Types (Money, Hand)

data Action = 
    Hit 
    | Stand 
    | Double 
    | Split 
    deriving (Show, Eq)

takeAction :: Action -> (Hand, bet) -> [(Hand, bet)]
takeAction Hit (h, b) = do
    newCard <- drawCard
    [(newCard ++ h, b)]
takeAction Stand (h, b) = [(h, b)]
takeAction Double (h, b) = if length h == 2
                                then do
                                    newCard <- drawCard
                                    [(newCard ++ h, b * 2)]
                                else do
                                    [(h,b)]
takeAction Split (h, b) = if length h == 2 && head h == head (tail h)
                                then do
                                    newCard1 <- drawCard
                                    newCard2 <- drawCard
                                    [(newCard1 : [head h], b), (newCard2 : [head (tail h)], b)]
                                else do
                                    [(h,b)]