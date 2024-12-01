{-# LANGUAGE NoImplicitPrelude #-}

module Lib where

import Data.Text (pack)
import Data.Text.Read (decimal, signed)
import RIO
import Text.Parsec.Text (Parser)

newtype Env = Env
    { logFunc :: LogFunc
    }

instance HasLogFunc Env where
    logFuncL = lens logFunc (\n s -> n{logFunc = s})

readInt :: String -> Either String Int
readInt = fmap fst . decimal . pack

parseInt :: String -> Parser Int
parseInt = either fail pure . readSignedInt

readSignedInt :: String -> Either String Int
readSignedInt = fmap fst . signed decimal . pack

parseSignedInt :: String -> Parser Int
parseSignedInt = either fail pure . readSignedInt
