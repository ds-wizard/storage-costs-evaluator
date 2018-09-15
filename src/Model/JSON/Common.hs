module Model.JSON.Common where

import Data.Aeson
import Control.Monad
import Text.Read

import Model.Common
import Model.JSON.Util

instance ToJSON Volume where
  toJSON Volume {..} =
    object
      [ "volumeValue" .= _volumeValue
      , "volumeUnit" .= show _volumeUnit
      ]

instance FromJSON Volume where
  parseJSON (Object o) = do
    _volumeValue <- o .: "volumeValue"
    _volumeUnit <- parseWithRead o "volumeUnit"
    return Volume {..}
  parseJSON _ = mzero
