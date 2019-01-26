module Util where

import           Data.Aeson

jsonOptions = defaultOptions
  { constructorTagModifier = camelTo2 '_'
  , fieldLabelModifier = camelTo2 '_'
  }
