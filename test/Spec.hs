import Deck 
import Game
import System.Random
import Control.Monad
import Control.Monad.Trans.State
import Control.Monad.Trans.Class
import Text.Read (readMaybe)
import Data.Maybe (catMaybes)
import Control.Lens

testDealer :: Player
testDealer = Dealer
    { playerName = "testDealer"
    , hand = []
    }

testPlayer1 :: Player
testPlayer1 = Player
    { playerName = "Jake"
    , hands = [[]]
    , bankroll = 100
    , bet = [1,2,3]
    }

testPlayer2 :: Player
testPlayer2 = Player
    { playerName = "Josh"
    , hands = [[]]
    , bankroll = 100
    , bet = [1,0,-1,23]
    }

testGame :: Game
testGame = Game
    { deck        = genDecks 2
    , discard     = []
    , dealer      = testDealer
    , players     = [testPlayer1, testPlayer2]
    , penetration = 66
    , gen         = mkStdGen 0}

newGame :: Game
newGame = execState dealOpeningHands testGame

main :: IO ()
main = print newGame