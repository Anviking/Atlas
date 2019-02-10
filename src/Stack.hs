{-# LANGUAGE OverloadedStrings #-}
module Stack where

import           Annotation
import           Control.Monad                    (forM_, when)
import           Data.Attoparsec.ByteString.Char8
import qualified Data.Attoparsec.ByteString.Char8 as P
import           Data.Attoparsec.Combinator
import qualified Data.ByteString.Char8            as C8
import           Data.Maybe                       (catMaybes)
import           Text.Read                        (readMaybe)

stringToColon = P.takeWhile (\x -> x /= ':' && x /= '\n')

restOfLine = P.takeWhile (/= '\n')

-- "foo/bar.hs:12:2: "
status :: Parser Annotation
status = do
  file <- stringToColon
  _ <- ":"
  line <- decimal
  _ <- ":"
  column <- decimal
  _ <- ": "
  _ <- "error:"
  message <- restOfLine
  (a, b) <- m line

  return (C8.unpack file, line, column, C8.unpack message)
  return $ Annotation
    { path = C8.unpack file
    , startLine = line
    , endLine = line
    , startColumn = Just a
    , endColumn = Just b
    , message = C8.unpack message
    , annotationLevel = Failure
    }


m :: Int -> Parser (Int, Int)
m line = do
  _ <- "\n"
  head . catMaybes <$> guide `sepBy` (char '\n')

  where
    --    |
    -- 62 | some code
    --    |      ^^^^^
    guide = do
      x <- manyTill anyChar (char '|')
      if ((Just line) == (readMaybe x))
      then do
        -- skip to same column on next line
        let guideLength = length x
        _ <- restOfLine
        _ <- "\n"
        _ <- P.take guideLength
        _ <- char '|'

        -- get the interval ([a,b]) for the ^^^^ thing
        loc <- length <$> P.many' (char ' ')
        len <- length <$> many1 (char '^')
        return $ Just (loc, loc + len)

      else do
        _ <- restOfLine
        return Nothing

output = do
  let a = string "ch"
  ((choice [Just <$> status, restOfLine >> return Nothing]) `sepBy` (char '\n'))

test = do
  f <- readFile "test/fixtures/stack-build.txt"
  case catMaybes <$> parseOnly Stack.output (C8.pack f) of
    Right a -> forM_ a print
    Left  e -> print e



