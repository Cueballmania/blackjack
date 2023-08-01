module Main (main) where

import Game ( makeGame, playGame, promptForPlayers )
import Control.Monad.Trans.State ( evalStateT )

main :: IO ()
main = do
    playerNames <- promptForPlayers
    game <- makeGame playerNames
    evalStateT playGame game
    return ()