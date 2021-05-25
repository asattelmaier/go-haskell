{-# LANGUAGE OverloadedStrings #-}



module API.JSON.Input.Settings
( Settings
) where



import           Data.Aeson  (FromJSON, Value (Object), parseJSON, (.:?))
import           Go.Settings (Settings (Settings))



instance FromJSON Settings where
  parseJSON (Object v) = Settings
    <$> v .:? "boardSize"
    <*> v .:? "isSuicideAllowed"

