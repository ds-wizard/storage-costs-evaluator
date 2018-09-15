module Model.JSON.Util where

import Data.Aeson
import Data.Aeson.Types
import Data.Text
import Text.Read


parseWithRead :: Read a => Object -> Text -> Parser a
parseWithRead o field = do
  maybeValue <- readMaybe <$> (o .: field)
  case maybeValue of
    Just value -> return value
    _ -> fail errorMessage
  where
    errorMessage = "Unkown " ++ unpack field
