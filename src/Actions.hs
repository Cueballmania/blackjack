module Actions where

import Types (Money, Hand, Game (..), GameT, Player (..))
import Control.Monad.Trans.State
import Deck (Card, shuffle, cardValue)
import Control.Monad.IO.Class (liftIO)

data Action =
    Hit
    | Stand
    | Double
    | Split
    deriving (Show, Eq)

getValidTurnAction :: (Hand, Money) -> Money -> IO Action
getValidTurnAction (h, b) br = do
    (prompt, actions) <- turnPrompt (h, b) br
    putStrLn prompt
    action <- getLine
    let turnAction "H" = return Hit
        turnAction "S" = return Stand
        turnAction "D" = return Double
        turnAction "P" = return Split
        turnAction _ = do
            putStrLn "Invalid action. Try again."
            getValidTurnAction (h, b) br
    validAction <- turnAction action
    if validAction `elem` actions
        then return validAction
        else do
            putStrLn "Invalid action. Try again."
            getValidTurnAction (h, b) br


turnPrompt :: (Hand, Money) -> Money -> IO (String, [Action])
turnPrompt (h, b) br =
    let
        prompt = if br >= b && length h == 2
                    then if cardValue (head h) == cardValue (head (tail h))
                        then "What would you like to do? (H)it, (S)tand, (D)ouble, S(P)lit"
                        else "What would you like to do? (H)it, (S)tand, (D)ouble"
                    else "What would you like to do? (H)it, (S)tand"
        actions = if br >= b && length h == 2
                    then if cardValue (head h) == cardValue (head (tail h))
                        then [Hit, Stand, Double, Split]
                        else [Hit, Stand, Double]
                    else [Hit, Stand]
    in
        return (prompt, actions)


playerTurn :: Player -> GameT IO Player
playerTurn p = do
    let aHands = activeHands p
    let pHands = playedHands p
    let br = bankroll p
    case aHands of
        [] -> return p
        ((h, b):hs) -> do
            _ <- liftIO $ putStrLn $ "It's " ++ playerName p ++ "'s turn."
            _ <- liftIO $ putStrLn $ "Hand: " ++ show h ++  " Value: " ++ show (sum (map cardValue h))
            _ <- liftIO $ putStrLn $ "Bet: " ++ show b
            action <- liftIO $ getValidTurnAction (h,b) br
            case action of
                Stand -> do
                    _ <- liftIO $ putStrLn $ "Player " ++ playerName p ++ " stands with hand: " ++ show h ++  " Value: " ++ show (sum (map cardValue h))
                    (playedHand, newState) <- runState (takeAction action (h,b)) <$> get
                    put newState
                    let newPlayer = p { playedHands = pHands ++ playedHand, activeHands = hs }
                    playerTurn newPlayer
                Double -> do
                    (playedHand, newState) <- runState (takeAction action (h,b)) <$> get
                    put newState
                    let handValue = sum (map cardValue (fst $ head playedHand))
                    _ <- liftIO $ putStrLn $ "Player " ++ playerName p ++ " doubles to hand: " ++ show (fst $ head playedHand) ++  " Value: " ++ show handValue
                    let newPlayer = p { playedHands = pHands ++ playedHand, activeHands = hs, bankroll = br - b }
                    playerTurn newPlayer
                Hit -> do
                    _ <- liftIO $ putStrLn $ "Player " ++ playerName p ++ " hits!"
                    (updatedHand, newState) <- runState (takeAction action (h,b)) <$> get
                    put newState
                    let handValue = sum (map cardValue (fst $ head updatedHand))
                    if handValue > 21
                        then do
                            _ <- liftIO $ putStrLn $ "Bust Hand! with " ++ show (fst $ head updatedHand) ++ " Value: " ++ show handValue
                            let newPlayer = p { playedHands = pHands ++ updatedHand, activeHands = hs }
                            playerTurn newPlayer
                        else do
                            _ <- liftIO $ putStrLn $ "Now has: " ++ show h ++  " Value: " ++ show handValue
                            let newPlayer = p { playedHands = pHands, activeHands = updatedHand ++ hs }
                            playerTurn newPlayer
                Split -> do
                    _ <- liftIO $ putStrLn $ "Player " ++ playerName p ++ " splits " ++ show (cardValue $ head h)
                    (newHand, newState) <- runState (takeAction action (h,b)) <$> get
                    put newState
                    let newPlayer = p { activeHands = newHand ++ hs, bankroll = br - b }
                    playerTurn newPlayer


takeAction :: Action -> (Hand, Money) -> State Game [(Hand, Money)]
takeAction Stand (h, b) = return [(h, b)]
takeAction Hit (h, b) = do
    newCard <- drawCard
    return [(h ++ [newCard], b)]
takeAction Double (h, b) = if length h == 2
                                then do
                                    newCard <- drawCard
                                    return [(newCard : h, b * 2)]
                                else do
                                    return [(h,b)]
takeAction Split (h, b) = if length h == 2 && cardValue (head h) == cardValue (head (tail h))
                                then do
                                    newCard1 <- drawCard
                                    newCard2 <- drawCard
                                    return [(newCard1 : [head h], b), (newCard2 : [head (tail h)], b)]
                                else do
                                    return [(h,b)]

drawCard :: State Game Card
drawCard = do
    gs <- get
    case deck gs of
        [] -> do
            let (newDeck, newGen) = shuffle (discard gs) (gen gs)
            put $ gs { deck = newDeck, discard = [], gen = newGen }
            drawCard
        (c:cs) -> do
            put $ gs { deck = cs }
            return c
