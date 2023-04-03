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

drawN :: Int -> ([[Card]], Deck, Deck, StdGen) -> ([[Card]], Deck, Deck, StdGen)
drawN 0 (c, dk, dc, g) = (c, dk, dc, g)
drawN n (c, dk, dc, g) = drawN (n-1) (c ++ [[c']], dk', dc', g')
    where
    (c', (dk', dc', g')) = drawCard (dk, dc, g)

drawCard :: (Deck, Deck, StdGen) -> (Card, (Deck, Deck, StdGen))
drawCard (c:d, discard, g) = (c, (d, discard, g))
drawCard ([], discard, g)  = drawCard (newDeck, [], h)
    where
        (newDeck, h) = shuffle discard g

dealToAllPlayers :: ([Player], Deck, Deck, StdGen) -> [Player] -> ([Player], Deck, Deck, StdGen)
dealToAllPlayers (processedPlayers, dk, dc, g) []     = (processedPlayers, dk, dc, g)
dealToAllPlayers (processedPlayers, dk, dc, g) (p:ps) =  dealToAllPlayers (processedPlayers ++ [p'], dk', dc', g') ps
    where
        (p', dk', dc', g') = dealToPlayer p (dk, dc, g)

dealToPlayer :: Player -> (Deck, Deck, StdGen) ->  (Player, Deck, Deck, StdGen)
dealToPlayer p (dk, dc, g) = (p', dk', dc', g')
    where
        numHands = length $ bet p
        (newHands, dk', dc', g') = drawN numHands ([], dk, dc, g)
        p' = if hands p == [[]]
                then p { hands = newHands}
                else p { hands = zipWith (++) (hands p) newHands}

dealOpeningHands :: [Player] -> Dealer -> (Deck, Deck, StdGen) -> ([Player], Dealer, Deck, Deck, StdGen)
dealOpeningHands ps d (deck, discard, gen) = 
    let (firstCardPlayers, dk, dc, g) = dealToAllPlayers ([], deck, discard, gen) ps
        (dealerCard, (dk', dc', g')) = drawCard (dk, dc, g)
        (secondCardPlayers, dk'', dc'', g'') = dealToAllPlayers([], dk', dc', g') firstCardPlayers
        (dealerHiddenCard, (dk''', dc''', g''')) = drawCard (dk'', dc'', g'')
        newDealer = d {hand = [dealerCard], hiddenHand = [dealerHiddenCard] }       
    in (secondCardPlayers, newDealer, dk''', dc''', g''')

playGame :: GameT IO ()
playGame = do
    game <- get
    playerWBets <- liftIO $ getBets (players game)
    let dealer' = dealer game
    let (playerInitial, dealerInitial, dk, dc, g) = dealOpeningHands playerWBets dealer' (deck game, discard game, gen game)
    let game' = game { players = playerInitial, dealer = dealerInitial, deck = dk, discard = dc, gen = g }
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