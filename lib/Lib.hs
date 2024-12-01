{-# LANGUAGE NoImplicitPrelude #-}

module Lib where

import Data.Text.Read (decimal, signed)
import RIO

newtype Env = Env
    { logFunc :: LogFunc
    }

instance HasLogFunc Env where
    logFuncL = lens logFunc (\n s -> n{logFunc = s})

readInt :: Text -> Either String Int
readInt = fmap fst . decimal

readSignedInt :: Text -> Either String Int
readSignedInt = fmap fst . signed decimal
