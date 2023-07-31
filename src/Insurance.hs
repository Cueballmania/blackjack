module Insurance where

import Types (Dealer(..), Player (..), Game(..), GameT)
import Deck (cardValue)
import BlackjackRules
import Control.Monad.Trans.State
import Control.Monad
import Control.Monad.IO.Class

dealerBlackjack :: Dealer -> Bool
dealerBlackjack d = length h == 2 && handValue h == 21
    where
        h = hand d ++ hiddenHand d

calcMaxInsurance :: Player -> Int
calcMaxInsurance p = min (bankroll p) (div (sum $ map snd $ activeHands p) 2)

getValidInsuranceBet :: Int -> IO Int
getValidInsuranceBet maxBet = do
    putStrLn "Would you like to buy insurance? (Y/N)"
    response <- getLine
    case response of
        "Y" -> do
            putStrLn $ "You can bet up to " ++ show maxBet ++ "\nHow much would you like to bet?"
            bet <- read <$> getLine
            if bet > maxBet
                then do
                    putStrLn $ "You can't bet more than " ++ show maxBet
                    getValidInsuranceBet maxBet
                else return $ max bet 0
        "N" -> return 0
        _ -> do
            putStrLn "Invalid response. Try again."
            getValidInsuranceBet maxBet

processInsurance :: GameT IO ()
processInsurance = do
    g <- get
    let ps = players g
    let d = dealer g
    newPlayers <- forM ps $ \p -> do
        let maxBet = calcMaxInsurance p
        if maxBet > 0
            then do
                bet <- liftIO $ getValidInsuranceBet maxBet
                return p { insurance = bet, bankroll = bankroll p - bet }
            else return p
    if dealerBlackjack d 
        then do
            _ <- liftIO $ putStrLn "Dealer has blackjack. Insurance pays 2:1."
            let newPlayers2 = map (\p -> p { bankroll = bankroll p + 2 * insurance p, insurance = 0 }) newPlayers
            put $ g { players = newPlayers2 }
        else do
            _ <- liftIO $ putStrLn "Dealer does not have blackjack. Insurance lost."
            let newPlayers2 = map (\p -> p { bankroll = bankroll p, insurance = 0 }) newPlayers
            put $ g { players = newPlayers2 }