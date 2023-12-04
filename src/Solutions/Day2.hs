module Solutions.Day2
  ( aoc2
  ) where

import           Common.AoCSolutions (AoCSolution (MkAoCSolution),
                                      printSolutions, printTestSolutions)
import           Text.Parser.Combinators (some, try)
import           Text.Parser.Token       (integer)
import           Text.Trifecta           (CharParsing (anyChar, string, char), Parser,
                                          Parsing (skipMany), commaSep,newline, many,
                                          optional, whiteSpace,letter, sepBy, token, sepEndBy, eof, manyTill)
import           Control.Applicative
import           Data.List (map)

aoc2 :: IO ()
aoc2 = do
  printSolutions 2 $ MkAoCSolution parseInput part1
  printSolutions 2 $ MkAoCSolution parseInput part2


data Colour = Blue | Red | Green deriving (Enum, Show, Eq)
allColours = [Blue, Red, Green]

data Game
  = Game 
    { gameId :: Integer,
    rounds :: [Round]
    } deriving (Show)


type Handful = (Colour, Integer)
type Round = [Handful]

parseInput :: Parser [Game]
parseInput = do
  some $ token parseGame

parseGame :: Parser Game
parseGame = do
  string "Game "
  id <- integer
  string ":"
  rounds <- sepBy parseRound $ char ';'
  pure $ Game id rounds

parseRound :: Parser Round
parseRound = do
  commaSep parseHandful

parseHandful :: Parser Handful
parseHandful = do
  whiteSpace
  n <- integer
  whiteSpace
  colourStr <- many letter
  colour <- parseCol colourStr
  pure (colour, n)
  where
      parseCol c =
        case c of
          "blue" -> pure Blue
          "red" -> pure Red
          "green" -> pure Green
          c   -> fail $ "Unrecognised colour" ++ c




part1 :: [Game] -> Integer
part1 = sum . map gameId . filter (not . any includesLimitBreaker . concat . rounds)

includesLimitBreaker :: Handful -> Bool
includesLimitBreaker (Blue, n) = n > 14
includesLimitBreaker (Red, n) = n > 12
includesLimitBreaker (Green, n) = n > 13

part2 :: [Game] -> Integer
part2  = sum . map gamePower

gamePower :: Game -> Integer
gamePower g = product $ map (maxBalls handfuls) allColours
  where 
    handfuls = concat $ rounds g

maxBalls :: [Handful] -> Colour -> Integer
maxBalls handfuls colour =  case handfulsOfColour of
  [] -> 0
  l -> maximum l
  where 
    handfulsOfColour = map snd $ filter (\(c, _) -> c == colour) handfuls 