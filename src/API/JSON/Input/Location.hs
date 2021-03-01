{-# LANGUAGE OverloadedStrings #-}



module API.JSON.Input.Location
( Location
) where



import Data.Aeson
import Control.Applicative
import Go.Board



instance FromJSON Location where
  parseJSON (Object v) = Location
    <$> v .: "x"
    <*> v .: "y"
  parseJSON _          = empty

