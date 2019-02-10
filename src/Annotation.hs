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


instance ToJSON   Level      where toJSON    = genericToJSON jsonOptions
instance FromJSON Level      where parseJSON = genericParseJSON jsonOptions

instance ToJSON   Annotation where toJSON    = genericToJSON jsonOptions
instance FromJSON Annotation where parseJSON = genericParseJSON jsonOptions
