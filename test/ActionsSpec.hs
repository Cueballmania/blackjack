module ActionsSpec where

import Test.Hspec
import Control.Monad.Trans.State
import Actions (takeAction, Action)
import Types
import Deck

-- Test cases for takeAction function
takeActionSpec :: Spec
takeActionSpec = do
    describe "takeAction" $ do
        it "returns the hand and bet when given Stand action" $ do
            let result = evalState (takeAction Stand ([Card Two Hearts, Card Three Hearts], 10)) newGame
            result `shouldBe` [([Card Two Hearts, Card Three Hearts], 10)]
        it "returns the hand with a new card when given Hit action" $ do
            let result = evalState (takeAction Hit ([Card Two Hearts, Card Three Hearts], 10)) newGame
            length (fst $ head result) `shouldBe` 3
        it "returns the hand with a new card and double the bet when given Double action with 2 cards" $ do
            let result = evalState (takeAction Double ([Card Two Hearts, Card Three Hearts], 10)) newGame
            result `shouldBe` [([Card Two Hearts, Card Three Hearts, Card Ace Hearts], 20)]
        it "returns the hand and bet when given Double action with more than 2 cards" $ do
            let result = evalState (takeAction Double ([Card Two Hearts, Card Three Hearts, Card Four Hearts], 10)) newGame
            result `shouldBe` [([Card Two Hearts, Card Three Hearts, Card Four Hearts], 10)]
        it "returns two hands with one card each and the same bet when given Split action with two cards of the same value" $ do
            let result = evalState (takeAction Split ([Card Two Hearts, Card Two Spades], 10)) newGame
            result `shouldBe` [([Card Two Hearts], 10), ([Card Two Spades], 10)]
        it "returns the hand and bet when given Split action with two cards of different values" $ do
            let result = evalState (takeAction Split ([Card Two Hearts, Card Three Spades], 10)) newGame
            result `shouldBe` [([Card Two Hearts, Card Three Spades], 10)]

-- Test cases for playerTurn function
playerTurnSpec :: Spec
playerTurnSpec = do
    describe "playerTurn" $ do
        it "returns the same player when given an empty active hands list" $ do
            let player = Player "Test" [] [] 100
            let result = evalState (playerTurn player) newGame
            result `shouldBe` player
        it "returns a new player with the played hand and an empty active hands list when given a Stand action" $ do
            let player = Player "Test" [([Card Two Hearts, Card Three Hearts], 10)] [] 100
            let result = evalState (playerTurn player) newGame
            let expectedPlayer = Player "Test" [] [([Card Two Hearts, Card Three Hearts], 10)] 100
            result `shouldBe` expectedPlayer
        it "returns a new player with the played hand and an empty active hands list when given a Double action" $ do
            let player = Player "Test" [([Card Two Hearts, Card Three Hearts], 10)] [] 100
            let result = evalState (playerTurn player) newGame
            let expectedPlayer = Player "Test" [] [([Card Two Hearts, Card Three Hearts, Card Ace Hearts], 0)] 90
            result `shouldBe` expectedPlayer
        it "returns a new player with the updated active hands list when given a Hit action that doesn't bust" $ do
            let player = Player "Test" [([Card Two Hearts, Card Three Hearts], 10)] [] 100
            let result = evalState (playerTurn player) newGame
            let expectedPlayer = Player "Test" [([Card Two Hearts, Card Three Hearts, Card Four Hearts], 10)] [] 100
            result `shouldBe` expectedPlayer
        it "returns a new player with the played hand and an empty active hands list when given a Hit action that busts" $ do
            let player = Player "Test" [([Card Ten Hearts, Card Jack Hearts], 10)] [] 100
            let result = evalState (playerTurn player) newGame
            let expectedPlayer = Player "Test" [] [([Card Ten Hearts, Card Jack Hearts, Card Two Hearts], 0)] 100
            result `shouldBe` expectedPlayer
        it "returns a new player with two active hands when given a Split action with two cards of the same value" $ do
            let player = Player "Test" [([Card Two Hearts, Card Two Spades], 10)] [] 100
            let result = evalState (playerTurn player) newGame
            let expectedPlayer = Player "Test" [([Card Two Hearts], 10), ([Card Two Spades], 10)] [] 90
            result `shouldBe` expectedPlayer

-- Test cases for drawCard function
drawCardSpec :: Spec
drawCardSpec = do
    describe "drawCard" $ do
        it "returns a card and updates the deck when the deck is not empty" $ do
            let result = evalState drawCard newGame
            length (deck result) `shouldBe` 51
        it "shuffles the discard pile and returns a card when the deck is empty" $ do
            let game = newGame { deck = [] }
            let result = evalState drawCard game
            length (deck result) `shouldBe` 51
            length (discard result) `shouldBe` 0
            penetration result `shouldBe` 102

-- Test cases for turnPrompt function
turnPromptSpec :: Spec
turnPromptSpec = do
    describe "turnPrompt" $ do
        it "returns the correct prompt and actions when the player can double and split" $ do
            let result = evalState (turnPrompt ([Card Two Hearts, Card Two Spades], 10) 20) newGame
            fst result `shouldBe` "What would you like to do? (H)it, (S)tand, (D)ouble, S(P)lit"
            snd result `shouldBe` [Hit, Stand, Double, Split]
        it "returns the correct prompt and actions when the player can only double" $ do
            let result = evalState (turnPrompt ([Card Two Hearts, Card Three Spades], 10) 20) newGame
            fst result `shouldBe` "What would you like to do? (H)it, (S)tand, (D)ouble"
            snd result `shouldBe` [Hit, Stand, Double]
        it "returns the correct prompt and actions when the player can only hit and stand" $ do
            let result = evalState (turnPrompt ([Card Two Hearts, Card Three Spades, Card Four Hearts], 10) 20) newGame
            fst result `shouldBe` "What would you like to do? (H)it, (S)tand"
            snd result `shouldBe` [Hit, Stand]


-- Test cases for cardValue function
cardValueSpec :: Spec
cardValueSpec = do
    describe "cardValue" $ do
        it "returns the correct value for numbered cards" $ do
            let result = cardValue (Card Two Hearts)
            result `shouldBe` 2
        it "returns 10 for face cards" $ do
            let result = cardValue (Card King Hearts)
            result `shouldBe` 10
        it "returns 11 for aces" $ do
            let result = cardValue (Card Ace Hearts)
            result `shouldBe` 11

-- Test cases for handValue function
handValueSpec :: Spec
handValueSpec = do
    describe "handValue" $ do
        it "returns the correct value for a hand with numbered cards" $ do
            let result = handValue [Card Two Hearts, Card Three Hearts, Card Four Hearts]
            result `shouldBe` 9
        it "returns the correct value for a hand with face cards" $ do
            let result = handValue [Card King Hearts, Card Queen Hearts]
            result `shouldBe` 20
        it "returns the correct value for a hand with an ace and a face card" $ do
            let result = handValue [Card Ace Hearts, Card King Hearts]
            result `shouldBe` 21
        it "returns the correct value for a hand with multiple aces" $ do
            let result = handValue [Card Ace Hearts, Card Ace Spades, Card Two Hearts]
            result `shouldBe` 14

-- Test cases for all functions
actionSpec :: Spec
actionSpec = do
    takeActionSpec
    playerTurnSpec
    drawCardSpec
    turnPromptSpec
    cardValueSpec
    handValueSpec
