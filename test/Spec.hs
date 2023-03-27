import Deck 
import Game
import System.Random
import Control.Monad.Trans.State

testDealer :: Dealer
testDealer = Dealer
    { dealerName = "testDealer"
    , hand = []
    , hiddenHand = []
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