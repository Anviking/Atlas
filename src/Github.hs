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
  , annotations :: Maybe [Annotation]
  } deriving stock (Show, Eq, Generic)
    deriving (ToJSON)

data CheckResponse = CheckResponse
  { id :: Int
  } deriving stock (Show, Eq, Generic)
    deriving (ToJSON, FromJSON)

data Annotation = Annotation
  { path             :: String
  , start_line       :: Int
  , end_line         :: Int
  , message          :: String
  , annotation_level :: String
  } deriving stock (Show, Eq, Generic)
    deriving (ToJSON)
