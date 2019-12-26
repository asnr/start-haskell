module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Parser

main :: IO ()
main = TIO.getContents >>= putStr . show . parseAll . T.filter (not . isWhitespace)

isWhitespace :: Char -> Bool
isWhitespace c = c == ' ' || c == '\n'
