{-# LANGUAGE OverloadedStrings #-}



module API.JSON.Input.Location
( Location
) where



import           Control.Applicative
import           Data.Aeson
import           Go.Board



instance FromJSON Location where
  parseJSON (Object v) = Location
    <$> v .: "x"
    <*> v .: "y"
  parseJSON _          = empty

