module Game where

import Deck 
import Types
import Bets
import System.Random
import Control.Monad.Trans.State
import Control.Monad.IO.Class (liftIO)

printTableValue :: [Card] -> IO ()
printTableValue cs = putStrLn $ "The value of cards is " ++ show vals
    where vals = sum [cardValue c | c <- cs]

shuffleDeck :: Monad m => GameT m Deck
shuffleDeck = do
    game <- get
    let (shufDeck, gen') = shuffle (deck game) (gen game)
    let game' = game {deck = shufDeck, gen = gen'}
    put game'
    return shufDeck

cutCard :: Monad m => GameT m Int
cutCard = do
    game <- get
    let dkSize = length (deck game)
    let (index, gen') = randomR ((0.35 * fromIntegral dkSize) :: Double
                                ,(0.45 * fromIntegral dkSize) :: Double) (gen game)
    let pen = ceiling index
    let game' = game { penetration = pen, gen = gen' }
    put game'
    return pen

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

dealOpeningHands :: Monad m => GameT m ()
dealOpeningHands = do
    game <- get
    let (newPlayers, newDeck) = dealFirstCard ([], deck game) (players game)
    let (dealer1, newDeck') = dealerCard newDeck (dealer game)
    let (startPlayers, newDeck'') = startingHands ([], newDeck') newPlayers
    let (dealer2, newDeck''') = dealerCardH newDeck'' dealer1
    let game' = game { dealer = dealer2, players = startPlayers, deck = newDeck''' }
    put game'

dealerCard :: Deck -> Dealer -> (Dealer, Deck)
dealerCard d p = (p', d')
    where
        (c, d') = drawCard d
        dealerHand = hand p
        p' = p { hand = dealerHand ++ [c] }

dealerCardH :: Deck -> Dealer -> (Dealer, Deck)
dealerCardH d p = (p', d')
    where
        (c, d') = drawCard d
        p' = p { hiddenHand = [c] }


startingHands :: ([Player], Deck) -> [Player] -> ([Player], Deck)
startingHands (processedPlayers, d) []     = (processedPlayers, d)
startingHands (processedPlayers, d) (p:ps) = startingHands (processedPlayers ++ [p'], d') ps
    where
        (p', d') = dealSecondCard p d

dealSecondCard :: Player -> Deck -> (Player, Deck)
dealSecondCard p d = (p', d')
    where
        currentHands = hands p
        (newHands, d') = drawSecond currentHands d :: ([Hand], Deck)
        p' = p { hands = newHands }

drawSecond :: [Hand] -> Deck -> ([Hand], Deck)
drawSecond hs d = (newHands, d')
    where
        (newHands, d') = drawToHand ([], d) hs

drawToHand :: ([Hand], Deck) -> [Hand]  -> ([Hand], Deck)
drawToHand (hands, d) []     = (hands, d)
drawToHand (hands, d) (h:hs) =
    let (c, newDeck) = drawCard d
        (newHand, d') = if length h == 1
                            then (h ++ [c], newDeck)
                            else (h, d)
    in drawToHand (hands ++ [newHand], d') hs

dealFirstCard :: ([Player], Deck) -> [Player] -> ([Player], Deck)
dealFirstCard (processedPlayers, d) []     = (processedPlayers, d)
dealFirstCard (processedPlayers, d) (p:ps) =  dealFirstCard (processedPlayers ++ [p'], d') ps
    where
        (p', d') = dealFirstPlayer p d

dealFirstPlayer :: Player -> Deck -> (Player, Deck)
dealFirstPlayer p d = (p', d')
    where
        numHands = length $ filter (>0) $ bet p
        (newHands, d') = drawNInitial numHands ([], d) :: ([Hand], Deck)
        p' = p { hands = newHands }

drawNInitial :: Int -> ([[Card]], Deck) -> ([[Card]], Deck)
drawNInitial 0 (c, dk) = (c, dk)
drawNInitial n (c, dk) = drawNInitial (n-1) (c ++ [[c']], dk')
    where
        (c', dk') = drawCard dk

drawCard :: Deck -> (Card, Deck)
drawCard (c:d) = (c, d)
drawCard [] = error "Cannot draw from an empty deck"

playGame :: GameT IO ()
playGame = do
    game <- get
    playerWBets <- liftIO $ getBets (players game)
    let game' = game { players = playerWBets }
    liftIO $ print game'
    put game'
    -- cleanup bets
    -- deal opening hands
    -- if Ace/Ten, ask for insurance bets
        -- check for dealer 21
        -- resolve bets if 21
    -- for each player
        -- process each hand in order
        -- for each hand, if it's a blackjack, tell them and pay them the payout rate
        -- if not, ask if they want to hit, stand, double (if they have enough money) or 
            -- split (if the two cards have the same numerical value and they have enough money and the number of hands is less than 4)
        -- if they split, take the second card and make a new hand after the previous, debt their account and add the bet
            -- deal the current hand another card and repeat
            -- a two-card 21 is not a blackjack and pays 1:1
        -- if they double down, double their bet, debt the account and give them one card and stop prompts for the hand
        -- if they hit, give them another card and repeat
        -- if they stand, move to the next hand
        -- if anytime the player's hand totals more than 21, immediate take the bet and move the cards to the discard pile
    -- the dealer then moves the hidden card to their hand and continues to hit until the stopping criteria
    -- all remaining bets are resolved (push = money goes back, win = double money, loss = clearing of bets)
    -- move all cards to the discard pile
    -- if the size of the deck is less than the penetration size, shuffle all cards
        -- reset the cut card
    -- if at anytime the deck runs out, shuffle the discards