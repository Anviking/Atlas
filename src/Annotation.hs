{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
module Annotation where

import           Data.Aeson
import           GHC.Generics

import           Util

data Annotation = Annotation
  { path            :: String
  , startLine       :: Int
  , endLine         :: Int
  , startColumn     :: Maybe Int
  , endColumn       :: Maybe Int
  , message         :: String
  , annotationLevel :: Level
  } deriving stock (Show, Eq, Generic)

data Level
  = Notice
  | Warning
  | Failure
  deriving stock (Show, Eq, Generic)

omitJsonOptions = jsonOptions { omitNothingFields = True }

instance ToJSON   Level      where toJSON    = genericToJSON omitJsonOptions
instance FromJSON Level      where parseJSON = genericParseJSON omitJsonOptions

instance ToJSON   Annotation where toJSON    = genericToJSON omitJsonOptions
instance FromJSON Annotation where parseJSON = genericParseJSON omitJsonOptions
