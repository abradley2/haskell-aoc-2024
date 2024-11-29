{-# LANGUAGE NoImplicitPrelude #-}

module Lib (Env (..)) where

import RIO

newtype Env = Env
    { logFunc :: LogFunc
    }

instance HasLogFunc Env where
    logFuncL = lens logFunc (\n s -> n{logFunc = s})
