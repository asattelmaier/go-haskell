{-# LANGUAGE OverloadedStrings #-}



module API.JSON.Output.Location
( Location
) where



import Data.Aeson
import Go.Board



instance ToJSON Location where
  toJSON (Location x y) =
    object [ "x" .= x,
             "y" .= y ]
    
