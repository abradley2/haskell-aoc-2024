{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Lib (Env (..), parseInt)
import RIO
import RIO.HashMap qualified as HashMap
import RIO.List (sort)
import Text.Parsec qualified as Parsec
import Text.Parsec.Text (Parser)

newtype Error = Error Parsec.ParseError deriving (Show)
instance Exception Error

inputParser :: Parser [(Int, Int)]
inputParser = run []
  where
    run res = do
        l <- Parsec.many1 Parsec.digit >>= parseInt
        _ <- Parsec.many1 Parsec.space
        r <- Parsec.many1 Parsec.digit >>= parseInt
        let line = (l, r)
        (Parsec.eof >> pure (reverse $ line : res)) <|> (Parsec.newline >> run (line : res))

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

partOne :: ByteString -> RIO Env Int
partOne =
    fmap (calcPartOne . partition)
        . fromEither
        . mapLeft Error
        . Parsec.runParser inputParser () ""
        . decodeUtf8Lenient

calcPartTwo :: ([Int], HashMap Int Int) -> Int
calcPartTwo = run 0
  where
    run res (l : lnext, scoreMap) =
        run (res + l * HashMap.lookupDefault 0 l scoreMap) (lnext, scoreMap)
    run res _ = res

partTwo :: ByteString -> RIO Env Int
partTwo =
    fmap (calcPartTwo . second makeScoreMap . partition)
        . fromEither
        . mapLeft Error
        . Parsec.runParser inputParser () ""
        . decodeUtf8Lenient

makeScoreMap :: [Int] -> HashMap Int Int
makeScoreMap = run mempty
  where
    run scoreMap (v : next) = run (HashMap.alter (Just . maybe 1 (+ 1)) v scoreMap) next
    run scoreMap _ = scoreMap

main :: IO ()
main = do
    input <- readFileBinary "./day01/input.txt"
    logFileHandle <- openFile "./day01/debug.txt" ReadWriteMode
    logOptions <- logOptionsHandle logFileHandle True
    withLogFunc logOptions $ \logFunc -> do
        partOneOutput <- runRIO (Env logFunc) (partOne input)
        partTwoOutput <- runRIO (Env logFunc) (partTwo input)
        runSimpleApp
            $ logInfo ("Part One: " <> display partOneOutput)
            >> logInfo ("Part Two: " <> display partTwoOutput)
            >> hFlush logFileHandle
