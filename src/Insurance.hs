module Insurance where

import Types (Dealer(..), Player (..))
import Deck (cardValue)

dealerBlackjack :: Dealer -> Bool
dealerBlackjack d = length h == 2 && handValue == 21
    where
        h = hand d ++ hiddenHand d
        handValue = sum $ map cardValue h

calcMaxInsurance :: Player -> Int
calcMaxInsurance p = min (bankroll p) (div (sum $ map snd $ activeHands p) 2)

getValidInsuranceBet :: Int -> IO Int
getValidInsuranceBet maxBet = do
    putStrLn "Would you like to buy insurance? (Y/N)"
    response <- getLine
    case response of
        "Y" -> do
            putStrLn $ "You can bet up to" ++ show maxBet ++ "How much would you like to bet?"
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