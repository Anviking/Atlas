{-# LANGUAGE OverloadedStrings #-}
module Lib where

import           Control.Lens
import           Data.Aeson                   (toJSON)
import qualified Data.ByteString.Char8        as C8
import           Language.Haskell.Exts.SrcLoc (srcFilename, srcLine)
import           Language.Haskell.HLint
import           Network.Wreq
import           System.Environment           (getEnv)

import           Annotation                   (Annotation (..))
import qualified Annotation
import qualified Github



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
    , startLine = srcLine loc
    , endLine = srcLine loc
    , annotationLevel = Annotation.Warning
    , message = show s
    }
  where
    loc = suggestionLocation s

create :: Github.Check -> IO Github.CheckResponse
create c = do
  opts <- optsWith <$> getEnv "token"

  r <- asJSON =<< postWith opts url (toJSON c)
  return $ r ^. responseBody


checkHlint :: String -> IO Github.CheckResponse
checkHlint sha = do
  hints <- hlint ["src"]
  let ann = map toAnnotation hints
  let output = Github.Output "Title" "Summmary" ann
  let conclusion = Github.Success
  create $ Github.Check "HLint" sha (Just output) conclusion

