module Solutions.Day5
  ( aoc5
  ) where

import           Common.AoCSolutions (AoCSolution (MkAoCSolution),
                                      printSolutions, printTestSolutions)
import           Text.Trifecta       (Parser, CharParsing (string, anyChar), whiteSpace, manyTill, integer, newline, sepBy,  many, letter, Parsing (try))
import qualified Data.Map as Map
import Data.Foldable (find)
import Data.List.Split (chunksOf)

aoc5 :: IO ()
aoc5 = do
  printSolutions 5 $ MkAoCSolution parseInput part1
  printSolutions 5 $ MkAoCSolution parseInput part2

data Input = Input {
  seeds :: [Integer],
  maps :: Mappings
} deriving (Show)
type Mappings = Map.Map String Destination
type Destination = (String, [MapValues])

data MapValues = MapValues {
  destination :: Integer,
  source :: Integer,
  offset :: Integer
} deriving (Show)


parseInput :: Parser Input
parseInput = do
  string "seeds:"
  whiteSpace
  seeds <- sepBy integer whiteSpace 
  mappings <- many parseMapping
  pure $ Input seeds $ Map.fromList mappings

parseMapping :: Parser (String, Destination)
parseMapping = do 
  source <- many letter
  string "-to-"
  destination <- many letter
  whiteSpace
  string "map:"
  newline
  mappings <- sepBy parseMapValue whiteSpace
  pure (source, (destination, mappings))

parseMapValue :: Parser MapValues
parseMapValue = do
  destination <- integer
  whiteSpace
  source <- integer
  MapValues destination source <$> integer

part1 :: Input -> Integer
part1 (Input seeds maps) = minimumLocation seeds $ toMapValues maps "seed"

toMapValues :: Mappings -> String -> [[MapValues]]
toMapValues maps source = case destination of 
  Nothing -> []
  Just (d, mappings) -> mappings : toMapValues maps d
  where destination = Map.lookup source maps

minimumLocation :: [Integer]  -> [[MapValues]] -> Integer
minimumLocation seeds maps = minimum $ map (findLastChild maps) seeds

findLastChild :: [[MapValues]] -> Integer -> Integer
findLastChild xs value = foldl applyMappings value xs


applyMappings :: Integer -> [MapValues] -> Integer
applyMappings value = maybe value (\(MapValues destination source offset) -> destination + (value - source)) 
  . find (\(MapValues _ source offset) -> value >= source && value < source + offset)

part2 :: Input -> Integer
part2 (Input seeds maps) = checkLocation reverseMap seeds 0
  where 
    reverseMap = toMapValues (reverseMappings maps)  "location"

checkLocation :: [[MapValues]] -> [Integer] -> Integer -> Integer
checkLocation maps seeds value 
  | seedsContainsNumber seedNumber seeds = value
  | otherwise = checkLocation maps seeds (value + 1)
  where seedNumber = findLastChild maps value

seedsContainsNumber :: Integer -> [Integer] -> Bool
seedsContainsNumber seed = any (matchesChunk seed) . chunksOf 2

matchesChunk :: Integer -> [Integer] -> Bool
matchesChunk v [start, offset] = v >= start && v < start + offset
matchesChunk _ _ = False

reverseMappings:: Mappings -> Mappings
reverseMappings = Map.fromList . map (\(source, (destination, mappings)) -> (destination, (source, map reverseMapValues mappings))) . Map.toList

reverseMapValues :: MapValues -> MapValues
reverseMapValues (MapValues d s o) = MapValues s d o