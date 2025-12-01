{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Data.Either
import Data.HashMap.Lazy qualified as HashMap
import Lib (intParser)
import Relude
import Text.Parsec qualified as Parsec
import Text.Parsec.Text (Parser)

newtype Error = Error Parsec.ParseError deriving (Show)

instance Exception Error

inputParser :: Parser [(Int, Int)]
inputParser = do
  inputLines <- Parsec.many1 parseLine
  _ <- Parsec.eof
  pure inputLines
  where
    parseLine = do
      l <- intParser
      _ <- Parsec.many1 Parsec.space
      r <- intParser
      _ <- Parsec.optionMaybe Parsec.newline
      pure (l, r)

partition :: [(Int, Int)] -> ([Int], [Int])
partition = run [] []
  where
    run l r ((lval, rval) : next) = run (lval : l) (rval : r) next
    run l r [] = (sort $ reverse l, sort $ reverse r)

calcPartOne :: ([Int], [Int]) -> Int
calcPartOne = run 0
  where
    run result (l : lnext, r : rnext) = run (result + abs (l - r)) (lnext, rnext)
    run result _ = result

partOne :: Text -> Either Error Int
partOne =
  fmap (calcPartOne . partition)
    . first Error
    . Parsec.runParser inputParser () ""

calcPartTwo :: ([Int], HashMap Int Int) -> Int
calcPartTwo = run 0
  where
    run res (l : lnext, scoreMap) =
      run (res + l * HashMap.lookupDefault 0 l scoreMap) (lnext, scoreMap)
    run res _ = res

partTwo :: Text -> Either Error Int
partTwo =
  fmap (calcPartTwo . second makeScoreMap . partition)
    . first Error
    . Parsec.runParser inputParser () ""

makeScoreMap :: [Int] -> HashMap Int Int
makeScoreMap = run mempty
  where
    run scoreMap (v : next) = run (HashMap.alter (Just . maybe 1 (+ 1)) v scoreMap) next
    run scoreMap _ = scoreMap

main :: IO ()
main = do
  input <- decodeUtf8 <$> readFileLBS "./day01/input.txt"
  print @String $ "Part one: " <> show (partOne input)
  print (partTwo input)
