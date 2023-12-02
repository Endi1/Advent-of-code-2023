module Main (main) where

import Data.Char
import Data.List
import Data.Maybe (isJust, fromJust)

readInput :: String -> IO String
readInput day = readFile $ "/home/endi/code/aoc2023/inputs/" ++ day

-- ==================== Day 1 ====================

readLine :: String -> Maybe Int
readLine line =
  let
    digits = parseDigits line
  in
    case length digits of
      0 -> Nothing
      1 -> Just $ read $  head digits ++ head digits
      _ -> Just $ read $  head digits ++ last digits

parseDigits :: String -> [String]
parseDigits (x:xs)
  | isDigit x = [x] : parseDigits xs
  | "one" `isPrefixOf` (x:xs) = "1" : parseDigits xs
  | "two" `isPrefixOf` (x:xs) = "2" : parseDigits xs
  | "three" `isPrefixOf` (x:xs) = "3" : parseDigits xs
  | "four" `isPrefixOf` (x:xs) = "4" : parseDigits xs
  | "five" `isPrefixOf` (x:xs) = "5" : parseDigits xs
  | "six" `isPrefixOf` (x:xs) = "6" : parseDigits xs
  | "seven" `isPrefixOf` (x:xs) = "7" : parseDigits xs
  | "eight" `isPrefixOf` (x:xs) = "8" : parseDigits xs
  | "nine" `isPrefixOf` (x:xs) = "9" : parseDigits xs
  | otherwise = parseDigits xs
parseDigits [] = []

-- ==================== Day 1 ====================

main :: IO ()
main = do
  input <- readInput "day1"
  let numbers = map readLine $ lines input
  -- print numbers
  print $ sum $ map fromJust numbers
