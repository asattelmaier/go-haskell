{-# LANGUAGE OverloadedStrings #-}



module API.JSON.Input.State where



import           API.JSON.Input.Color ()
import           Data.Aeson
import           Go.Board



instance FromJSON State where
  parseJSON state = case state of
    "Empty" -> pure Empty
    "Black" -> pure (Stone Black)
    "White" -> pure (Stone White)

