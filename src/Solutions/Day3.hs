{-# LANGUAGE TupleSections #-}
module Solutions.Day3
  ( aoc3
  ) where


import           Control.Applicative
import           Common.AoCSolutions (AoCSolution (MkAoCSolution),
                                      printSolutions, printTestSolutions)
import           Text.Parser.Char        (oneOf)
import           Text.Parser.Combinators (some)
import           Text.Parser.Token       (token, symbol)
import           Text.Trifecta           (CharParsing (anyChar), Parser, digit)
import Text.Read
import Data.Maybe (isNothing)

aoc3 :: IO ()
aoc3 = do
  printSolutions 3 $ MkAoCSolution parseInput part1
  printSolutions 3 $ MkAoCSolution parseInput part2

type Point = (Integer, Integer)
type Number = (Integer, Point)
type Symbol = (String, Point)

data Grid = Grid {
  numbers :: [Number],
  symbols :: [Symbol]
} deriving (Show)

parseInput :: Parser Grid
parseInput = do
  lines <- some $ token $ some (digit <|> oneOf ".!@£#$%^&*()_+/=-")
  pure $ Grid (toNumbers lines) (toSymbols lines)

toSymbols:: [String] -> [Symbol]
toSymbols lines =  map (\(f, s) -> ([f], s)) noPoints
  where 
    withColumns = map (zip [0 .. ]) lines
    withRows = zip [0..] withColumns
    toPoints (row, columns) = map (\(column, n) -> (n, (column, row))) columns
    allPoints = concatMap toPoints withRows
    noNumbers = filter (isNothing . readMaybeInt . fst) allPoints
    noPoints = filter (\(a, _) -> a /= '.') noNumbers


toNumbers:: [String] -> [Number]
toNumbers lines =  concatMap toPoints withRows
  where 
    withColumns = map (zip [0 .. ]) lines
    finalNumberStates = map (foldl checkNextDigit emptyNumberState) withColumns
    allNumbers = map (\(numbers, final) -> case final of 
      Nothing -> numbers
      Just a -> numbers ++ [a]
      ) finalNumberStates
    withRows = zip [0..] allNumbers
    toPoints (row, columns) = map (\(column, n) -> (n, (column, row))) columns

type NumberState = ([(Integer, Integer)], Maybe (Integer, Integer))
emptyNumberState :: NumberState 
emptyNumberState = ([], Nothing)
checkNextDigit :: NumberState -> (Integer, Char) -> NumberState
checkNextDigit (done, Nothing) (i, c) = case possibleDigit of
  Nothing -> (done, Nothing)
  Just n -> (done, Just (i, n))
  where 
    possibleDigit = readMaybeInt c
checkNextDigit (done, Just a) (_, c) = case possibleDigit of
  Nothing -> (done ++ [a], Nothing)
  Just n -> (done, Just (fst a, (10 * snd a) + n))
  where 
    possibleDigit = readMaybeInt c



part1 :: Grid -> Integer
part1 (Grid numbers symbols) = sum . map fst $ filter (adjacentToSymbol (map snd symbols)) numbers 

adjacentToSymbol :: [Point] -> Number -> Bool
adjacentToSymbol symbolPoints = any (`elem` symbolPoints) . getAllNumberAdjacencies
  
getAllNumberAdjacencies:: Number -> [Point]
getAllNumberAdjacencies (n, start) = makeRow (-1) ++ makeRow 0 ++ makeRow 1
  where 
    end = fst start + toInteger (length (show n)) - 1
    cols = [fst start -1..end+1]
    makeRow y = map (, snd start + y) cols
    

part2 :: Grid -> Integer
part2 (Grid numbers symbols) = sum $ map (product . map fst) actualGears 
  where 
    possibleGears = map snd $ filter (\(s, _) -> s == "*") symbols
    numberPositions = map (\n -> (fst n, getAllNumberAdjacencies n)) numbers
    gearAdjacencies = map (\p -> filter (\(_, pos) -> p `elem` pos) numberPositions) possibleGears
    actualGears = filter (\ad -> length ad == 2) gearAdjacencies


readMaybeInt:: Char -> Maybe Integer
readMaybeInt c = readMaybe [c]
