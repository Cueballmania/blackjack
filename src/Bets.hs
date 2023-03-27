module Bets where

import Types
import Data.List.Split

promptForBets :: Player -> IO Player
promptForBets player = do
  let maxBets = 4 :: Int
  putStrLn $ "Enter up to " ++ show maxBets ++ " bets for player " ++ playerName player ++ " separated by commas:"
  betsStr <- getLine
  let bets = parseBets betsStr
  if isValidBets bets (bankroll player)
    then return $ player { bet = bets, bankroll = bankroll player - sum bets }
    else do
      putStrLn "Invalid bets. Please try again."
      promptForBets player

getBets :: [Player] -> IO [Player]
getBets [] = return []
getBets (p:ps) = do
  p' <- promptForBets p
  ps' <- getBets ps
  return (p':ps')

-- Prompts the user for up to `numBets` comma-separated bet amounts for a player
promptPlayerBetsStrs :: Int -> Player -> IO String
promptPlayerBetsStrs numBets player = do
  putStrLn $ "Enter up to " ++ show numBets ++ " bets for player " ++ playerName player ++ " separated by commas:"
  getLine

-- Checks if a list of bets is valid for a player
isValidBets :: [Money] -> Money -> Bool
isValidBets bets br = all (> 0) bets && length bets <= 4 && sum bets <= br

-- Parses a string of bets into a list of Money values
parseBets :: String -> [Money]
parseBets str = map read $ splitOn "," str