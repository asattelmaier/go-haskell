module API.JSON.Output.DTO
( DTO (DTO)
) where



import Data.Aeson
import API.JSON.Output.Game



newtype DTO = DTO Game



instance ToJSON DTO where
  toJSON (DTO game) = toJSON game

