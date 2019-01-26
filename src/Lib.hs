{-# LANGUAGE OverloadedStrings #-}
module Lib where

import           Data.Aeson                   (toJSON)
import qualified Data.ByteString.Char8        as C8
import           Github
import           Language.Haskell.Exts.SrcLoc
import           Language.Haskell.HLint
import           Network.Wreq

import           System.Environment           (getEnv)

-- Operators such as (&) and (.~).
import           Control.Lens



someFunc :: IO ()
someFunc = putStrLn "someFunc"

sha = "c76d10de5ba75e7acf836c27becc50f6bc2f32be"


optsWith token = defaults
  & auth ?~ oauth2Bearer (C8.pack token)
  & header "Accept" .~ ["application/vnd.github.antiope-preview+json"]


url = "https://api.github.com/repos/anviking/check-runs/check-runs"

toAnnotation :: Suggestion -> Annotation
toAnnotation s =
  Annotation
    { path = srcFilename loc
    , start_line = srcLine loc
    , end_line = srcLine loc
    , annotation_level = "warning"
    , message = show s
    }
  where
    loc = suggestionLocation s

create :: Check -> IO CheckResponse
create c = do
  opts <- optsWith <$> getEnv "token"
  hints <- hlint ["src"]

  let a = map toAnnotation hints

  r <- asJSON =<< postWith opts url (toJSON c)
  return $ r ^. responseBody
