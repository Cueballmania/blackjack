module Bets (
  getBets
) where

import Types
import Data.List.Split
import Data.Char (isDigit)

-- Asks for bets and puts all bets into activeHands for that player
promptForBets :: Player -> IO Player
promptForBets p = do
  let maxBets = 4 :: Int
  putStrLn $ "Enter up to " ++ show maxBets ++ " bets for player " ++ playerName p ++ " separated by commas:"
  betsStr <- getLine
  let bets = parseBets betsStr
  if isValidBets bets (bankroll p)
    then
      return $ p { activeHands = [([], b) | b <- bets], bankroll = bankroll p - sum bets }
    else do
      putStrLn "Invalid bets. Please try again."
      promptForBets p

-- Parses a string of bets into a list of Money values
parseBets :: String -> [Money]
parseBets str
  | all isDigitAndComma str = map read $ filter (not . null) $ splitOn "," str
  | otherwise = []
  where isDigitAndComma c = isDigit c || c == ','

-- Gets a bet string for each player
getBets :: [Player] -> IO [Player]
getBets [] = return []
getBets (p:ps) = do
  p' <- promptForBets p
  ps' <- getBets ps
  return (p':ps')

-- Checks if a list of bets is valid for a player
isValidBets :: [Money] -> Money -> Bool
isValidBets [] _ = False
isValidBets bets br = all (> 0) bets && length bets <= 4 && sum bets <= br