module Main where

import           Lib

main :: IO ()
main = do
  pwd <- getPwd
  print $ "Guessing that the project root dir is " <> pwd
  _ <- checkHlint
  _ <- checkStack (pwd ++ "/")
  return ()
