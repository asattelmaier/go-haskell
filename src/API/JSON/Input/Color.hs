{-# LANGUAGE OverloadedStrings #-}



module API.JSON.Input.Color
( Color
) where



import           Data.Aeson (FromJSON, parseJSON)
import           Go.Board   (Color (Black, White))



instance FromJSON Color where
  parseJSON color = case color of
    "Black" -> pure Black
    "White" -> pure White
    _       -> fail $ "Unknwon Color: " <> show color

