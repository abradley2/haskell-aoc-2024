{-# LANGUAGE NoImplicitPrelude #-}

module Lib where

import Data.Text (pack)
import Data.Text.Read (decimal, double, signed)
import Relude
import Text.Parsec qualified as Parsec
import Text.Parsec.Text (Parser)

intParser :: Parser Int
intParser = Parsec.many1 (Parsec.noneOf " \n") >>= either fail (pure . fst) . signed decimal . pack

floatParser :: Parser Double
floatParser = Parsec.many1 (Parsec.noneOf " \n") >>= either fail (pure . fst) . signed double . pack
