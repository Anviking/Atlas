{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
module Github where

import           Data.Aeson
import           GHC.Generics

data Check = Check
  { name     :: String
  , head_sha :: String
  , output   :: Maybe Output
  } deriving stock (Show, Eq, Generic)
    deriving (ToJSON)

data Output = Output
  { title       :: String
  , summary     :: String
  , annotations :: [Annotation]
  } deriving stock (Show, Eq, Generic)
    deriving (ToJSON)

data CheckResponse = CheckResponse
  { id :: Int
  } deriving stock (Show, Eq, Generic)
    deriving (ToJSON, FromJSON)

data Annotation = Annotation
  { path            :: String
  , startLine       :: Int
  , endLine         :: Int
  , message         :: String
  , annotationLevel :: Level
  } deriving stock (Show, Eq, Generic)

data Level
  = Notice
  | Warning
  | Failure
  deriving stock (Show, Eq, Generic)

jsonOptions = defaultOptions
  { constructorTagModifier = camelTo2 '_'
  , fieldLabelModifier = camelTo2 '_'
  }

instance ToJSON   Level      where toJSON    = genericToJSON jsonOptions
instance FromJSON Level      where parseJSON = genericParseJSON jsonOptions

instance ToJSON   Annotation where toJSON    = genericToJSON jsonOptions
instance FromJSON Annotation where parseJSON = genericParseJSON jsonOptions
