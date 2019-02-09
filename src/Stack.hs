{-# LANGUAGE OverloadedStrings #-}
module Stack where

import           Control.Monad                    (forM_)
import           Data.Attoparsec.ByteString.Char8
import qualified Data.Attoparsec.ByteString.Char8 as P
import           Data.Attoparsec.Combinator
import qualified Data.ByteString.Char8            as C8
import           Data.Maybe                       (catMaybes)
import           Text.Read                        (readMaybe)

stringToColon = P.takeWhile (\x -> x /= ':' && x /= '\n')

restOfLine = P.takeWhile (/= '\n')

-- "foo/bar.hs:12:2: "
status :: Parser (String, Int, Int, String)
status = do
  file <- stringToColon
  _ <- ":"
  line <- decimal
  _ <- ":"
  column <- decimal
  _ <- ": "
  _ <- "error:"
  message <- restOfLine
  return (C8.unpack file, line, column, C8.unpack message)

output = do
  let a = string "ch"
  ((choice [Just <$> status, restOfLine >> return Nothing]) `sepBy` (char '\n'))

test = do
  f <- readFile "test/fixtures/stack-build.txt"
  case catMaybes <$> parseOnly Stack.output (C8.pack f) of
    Right a -> forM_ a print
    Left  e -> print e



