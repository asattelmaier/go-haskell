{-# LANGUAGE OverloadedStrings #-}



module API.JSON.Input.Color where



import           Data.Aeson
import           Go.Board



instance FromJSON Color where
  parseJSON color = case color of
    "Black" -> pure Black
    "White" -> pure White
    _       -> fail $ "Unknwon Color: " <> show color

