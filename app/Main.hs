module Main (main) where

import Game
import Control.Monad.Trans.State

main :: IO ()
main = do
    playerNames <- promptForPlayers
    game <- makeGame playerNames
    evalStateT playGame game
    return ()