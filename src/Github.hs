{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
module Github where

import           Data.Aeson
import           Data.Time
import           GHC.Generics

import           Annotation   (Annotation (..))
import qualified Annotation
import           Util

data Check = Check
  { name        :: String
  , head_sha    :: String
  , output      :: Maybe Output
  , conclusion  :: Conclusion
  , completedAt :: Maybe UTCTime
  } deriving stock (Show, Eq, Generic)

data Conclusion
  = Success
  | Failure
  | Neutral
  | Cancelled
  | TimedOut
  | ActionRequired
  deriving stock (Show, Eq, Generic)



data Output = Output
  { title       :: String
  , summary     :: String
  , annotations :: [Annotation]
  } deriving stock (Show, Eq, Generic)

data CheckResponse = CheckResponse
  { id :: Int
  } deriving stock (Show, Eq, Generic)

--
-- JSON instances
--

instance ToJSON CheckResponse where toJSON = genericToJSON jsonOptions
instance FromJSON CheckResponse where parseJSON = genericParseJSON jsonOptions

instance ToJSON Output where toJSON = genericToJSON jsonOptions
instance FromJSON Output where parseJSON = genericParseJSON jsonOptions

instance ToJSON Conclusion  where toJSON = genericToJSON jsonOptions
instance FromJSON Conclusion where parseJSON = genericParseJSON jsonOptions

instance ToJSON Check where toJSON = genericToJSON jsonOptions
instance FromJSON Check where parseJSON = genericParseJSON jsonOptions
