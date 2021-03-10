{-# LANGUAGE RecordWildCards   #-}



module API.REST.Output.GameDTO
( GameDTO (GameDTO)
) where



import           API.JSON.Output.Intersection ()
import           Data.Aeson
import           Go.Game



newtype GameDTO = GameDTO Game



instance ToJSON GameDTO where
  toJSON (GameDTO Game {..}) = toJSON positions

