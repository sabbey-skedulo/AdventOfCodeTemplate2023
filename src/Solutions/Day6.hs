module Solutions.Day6
  ( aoc6
  ) where

import           Common.AoCSolutions (AoCSolution (MkAoCSolution),
                                      printSolutions)
import           Text.Trifecta       (Parser, CharParsing (string), whiteSpace, integer, sepBy)
import GHC.Float.RealFracMethods (ceilingFloatInteger, ceilingDoubleInteger, floorDoubleInteger)
import Control.Monad (join)
import Data.Bifunctor (Bifunctor(bimap))
import Text.Read (readMaybe)
import qualified Data.Maybe

aoc6 :: IO ()
aoc6 = do
  printSolutions 6 $ MkAoCSolution parseInput part1
  printSolutions 6 $ MkAoCSolution parseInput part2

parseInput :: Parser Races
parseInput = do
  string "Time:"
  whiteSpace
  times <- sepBy integer whiteSpace
  string "Distance:"
  whiteSpace
  distances <- sepBy integer whiteSpace
  pure $ zip times distances

type Time = Integer
type Distance = Integer
type Races = [(Time, Distance)]

part1 :: Races -> Integer
part1 = product . map bounds

bounds :: (Time, Distance) -> Integer
bounds (t, d) = upper + 1 - lower
  where 
    squareTerm = sqrt $ fromIntegral (t*t - 4*d)
    lower = ceilingDoubleInteger ((fromInteger t - squareTerm) / 2) 
    upper = floorDoubleInteger ((fromInteger t + squareTerm) / 2) 

part2 :: Races -> Integer
part2 = bounds . combineRaces

combineRaces :: Races -> (Time, Distance)
combineRaces = join bimap readInt .foldl (\(tStr, dStr) (t, d) -> (tStr ++ show t, dStr ++ show d)) ("", "")

readInt:: String -> Integer
readInt c = Data.Maybe.fromMaybe 0 (readMaybe c)