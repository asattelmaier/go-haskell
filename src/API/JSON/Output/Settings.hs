{-# LANGUAGE OverloadedStrings #-}



module API.JSON.Output.Settings where



import           Data.Aeson  (ToJSON, object, toJSON, (.=))
import           Go.Settings (Settings (Settings))



instance ToJSON Settings where
  toJSON (Settings size isSuicideAllowed) =
    object [ "boardSize"        .= size,
             "isSuicideAllowed" .= isSuicideAllowed ]

