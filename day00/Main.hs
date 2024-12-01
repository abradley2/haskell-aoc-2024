{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Lib (Env (..))
import RIO

partOne :: ByteString -> RIO Env Text
partOne _rawInput = do
    pure "Not Implemented"

partTwo :: ByteString -> RIO Env Text
partTwo _rawInput = do
    pure "Not Implemented"

main :: IO ()
main = do
    input <- readFileBinary "./day00/input.txt"
    logFileHandle <- openFile "./day00/debug.txt" ReadWriteMode
    logOptions <- logOptionsHandle logFileHandle True
    withLogFunc logOptions $ \logFunc -> do
        partOneOutput <- runRIO (Env logFunc) (partOne input)
        partTwoOutput <- runRIO (Env logFunc) (partTwo input)
        runSimpleApp
            $ logInfo ("Part One: " <> display partOneOutput)
            >> logInfo ("Part Two: " <> display partTwoOutput)
            >> hFlush logFileHandle
