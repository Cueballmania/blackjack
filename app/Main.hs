module Main (main) where

import Game
import Deck
import Control.Monad.Trans.State
import Types

main :: IO ()
main = do
    playerNames <- promptForPlayers
    game <- makeGame playerNames
    evalStateT playGame game
    return ()