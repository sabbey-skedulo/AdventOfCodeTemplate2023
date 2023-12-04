module Solutions.Day4
  ( aoc4
  ) where

import           Common.AoCSolutions (AoCSolution (MkAoCSolution),
                                      printSolutions, printTestSolutions)
import           Text.Trifecta       (Parser, some, TokenParsing (token), CharParsing (string), integer, commaSep, sepBy, whiteSpace)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Data.List (sortBy, sortOn)
import Data.Ord (comparing, Down (Down))

aoc4 :: IO ()
aoc4 = do
  printSolutions 4 $ MkAoCSolution parseInput part1
  printSolutions 4 $ MkAoCSolution parseInput part2

data Card = Card {
  cardId :: Integer,
  winners :: [Integer],
  numbers :: [Integer]
} deriving (Show)

parseInput :: Parser [Card]
parseInput = do
  some $ token parseCard

parseCard :: Parser Card
parseCard = do
  string "Card"
  whiteSpace
  id <- integer
  string ":"
  whiteSpace
  winners <- sepBy integer whiteSpace
  whiteSpace
  string "|"
  whiteSpace
  Card id winners <$> sepBy integer whiteSpace

part1 :: [Card] -> Integer
part1 = sum . map cardScore

cardScore :: Card -> Integer
cardScore c = case matchingCards c of
  0 -> 0
  x -> product $ replicate (x - 1) 2

matchingCards :: Card -> Int
matchingCards c = length (filter (`elem` winners c) $ numbers c)

part2 :: [Card] -> Integer
part2 =  sum . Map.elems .foldl countKids Map.empty . sortOn (Data.Ord.Down . cardId)

countKids :: Map Integer Integer -> Card -> Map Integer Integer
countKids counts card = case toInteger $ matchingCards card of
  0  -> Map.insert (cardId card) 1 counts
  x  -> Map.insert (cardId card) (toInteger (1 + sum (mapMaybe (\d -> Map.lookup (cardId card + d) counts) [1..x]))) counts