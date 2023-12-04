module Solutions.Day1
  ( aoc1
  ) where

import           Common.AoCSolutions (AoCSolution (MkAoCSolution),
                                      printSolutions, printTestSolutions)
import           Data.List           (tails, stripPrefix)
import           Text.Trifecta       (Parser, manyTill, CharParsing (anyChar), newline, many, sepBy)
import Common.ListUtils (window3, window2)
import Text.Read
import           Data.Maybe      (mapMaybe)

aoc1 :: IO ()
aoc1 = do
  printSolutions 1 $ MkAoCSolution parseInput part1
  printSolutions 1 $ MkAoCSolution parseInput part2

parseInput :: Parser [String]
parseInput = do
  many (manyTill anyChar newline)

part1 :: [String] -> Int
part1 = sum . map (toDigit . mapMaybe readMaybeInt)

toDigit :: [Int] -> Int
toDigit [] = 0
toDigit [x] = x * 10 + x
toDigit (x:xs) = x * 10 + last xs


part2 :: [String] -> Int
part2 = part1 . map doReplacements

doReplacements :: String -> String
doReplacements [] = ""
doReplacements (x:xs) = case replacements of
    [] -> x : doReplacements xs
    (replaced:_) -> head replaced : doReplacements xs
    where
      replacements = mapMaybe (uncurry (replaceDigits (x : xs))) inputs
    

inputs :: [(Char, String)]
inputs = [('1', "one"),('2', "two"),('3', "three"),('4', "four"),('5', "five"),('6', "six"),('7', "seven"),('8', "eight"),('9', "nine")]


replaceDigits:: String -> Char -> String  -> Maybe String
replaceDigits str n pref = fmap(n :) (stripPrefix pref str)


readMaybeInt:: Char -> Maybe Int
readMaybeInt c = readMaybe [c]
