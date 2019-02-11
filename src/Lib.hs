{-# LANGUAGE OverloadedStrings #-}
module Lib where

import           Control.Lens
import           Data.Aeson                       (encode, toJSON)
import           Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8            as C8
import           Data.Maybe                       (catMaybes)
import           Data.Time
import           Language.Haskell.Exts.SrcLoc     (srcFilename, srcLine)
import           Language.Haskell.HLint           (Suggestion, hlint,
                                                   suggestionLocation)
import           Network.Wreq
import           System.Environment               (getEnv)
import           System.Process

import           Annotation                       (Annotation (..))
import qualified Annotation                       as A
import qualified Annotation
import           Github                           (Conclusion (..))
import qualified Github
import qualified Stack



someFunc :: IO ()
someFunc = putStrLn "someFunc"

optsWith token = defaults
  & auth ?~ oauth2Bearer (C8.pack token)
  & header "Accept" .~ ["application/vnd.github.antiope-preview+json"]


url = "https://api.github.com/repos/anviking/Atlas/check-runs"

toAnnotation :: Suggestion -> Annotation
toAnnotation s =
  Annotation
    { path = srcFilename loc
    , startLine = srcLine loc
    , endLine = srcLine loc
    , startColumn = Nothing
    , endColumn = Nothing
    , annotationLevel = Annotation.Warning
    , message = show s
    }
  where
    loc = suggestionLocation s

create :: Github.Check -> IO Github.CheckResponse
create c = do
  opts <- optsWith <$> getEnv "token"
  print (encode c)
  r <- asJSON =<< postWith opts url (toJSON c)
  return $ r ^. responseBody


checkHlint :: IO Github.CheckResponse
checkHlint = do
  sha <- init <$> readProcess  "git" ["rev-parse", "HEAD"] []
  output <- hlintOutput
  let conclusion = defaultConclusion (Github.annotations output)
  time <- getCurrentTime
  create $ Github.Check "HLint" sha (Just output) conclusion (Just time)


defaultSummary :: [Annotation] -> String
defaultSummary _ = ""

defaultConclusion :: [Annotation] -> Github.Conclusion
defaultConclusion a = convert $ foldl reevaluate A.Notice (map A.annotationLevel a)
  where
    -- Todo: must be a better way with Ord or something
    reevaluate :: Annotation.Level -> Annotation.Level -> Annotation.Level
    reevaluate A.Notice a          = a
    reevaluate A.Warning A.Notice  = A.Warning
    reevaluate A.Warning A.Failure = A.Failure
    reevaluate A.Warning A.Warning = A.Failure
    reevaluate A.Failure _         = A.Failure

    convert :: Annotation.Level -> Conclusion
    convert A.Notice  = Neutral
    convert A.Warning = Failure
    convert A.Failure = Failure

hlintOutput :: IO Github.Output
hlintOutput = do
  hints <- hlint ["src"]
  let ann = map toAnnotation hints
  return $ Github.Output "Hlint" (defaultSummary ann) ann

getPwd = init <$> readProcess  "pwd" [] []


checkStack :: String -> IO Github.CheckResponse
checkStack prefix = do
  sha <- init <$> readProcess  "git" ["rev-parse", "HEAD"] []
  (_, _, log) <- readProcessWithExitCode "stack" ["build"] []
  print log
  ann <- case catMaybes <$> parseOnly (Stack.output prefix) (C8.pack log) of
    Right a -> return a
    Left  e -> print e >> return []
  let output = Github.Output "Title" "Summmary" ann
  let conclusion = defaultConclusion ann
  time <- getCurrentTime
  create $ Github.Check "stack build" sha (Just output) conclusion (Just time)


--checkBuild :: IO Github.Check
--checkBuild = do
--  output <- readProcess "stack" ["build"] []
--

--parseStack :: String -> [Annotation]
--parseStack =

