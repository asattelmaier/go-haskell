module API.JSON.Output.DTO
( DTO (DTO)
) where



import           API.JSON.Output.Game
import           Data.Aeson



newtype DTO = DTO Game



instance ToJSON DTO where
  toJSON (DTO game) = toJSON game

