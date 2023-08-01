{-# OPTIONS_GHC -F -pgmF hspec-discover #-}

import Test.Hspec
import ActionSpec

-- testDealer :: Dealer
-- testDealer = Dealer
--     { dealerName = "testDealer"
--     , hand = []
--     , hiddenHand = []
--     }

-- testPlayer1 :: Player
-- testPlayer1 = Player
--     { playerName = "Jake"
--     , hands = [[]]
--     , bankroll = 100
--     , bet = [1,2,3]
--     }

-- testPlayer2 :: Player
-- testPlayer2 = Player
--     { playerName = "Josh"
--     , hands = [[]]
--     , bankroll = 100
--     , bet = [1,23]
--     }

-- testGame :: Game
-- testGame = Game
--     { deck        = genDecks 2
--     , discard     = []
--     , dealer      = testDealer
--     , players     = [testPlayer1, testPlayer2]
--     , penetration = 66
--     , gen         = mkStdGen 0}

-- newGame :: Game -> IO Game
-- newGame t = do
--     (_, game) <- runStateT playGame t
--     return game

main :: IO ()
main = do
    hspec actionSpec
    
--     finalGame <- newGame testGame
--     putStrLn ("Final Game: " ++ show finalGame)
