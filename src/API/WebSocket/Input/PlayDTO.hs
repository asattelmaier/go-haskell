{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}



module API.WebSocket.Input.PlayDTO
( DTO (DTO)
, getGame
, getLocation
, fromData
) where



import           API.JSON.Input.Game
import           API.JSON.Input.Location
import qualified API.WebSocket.Input.Data as Data
import           Control.Applicative
import           Control.Lens



data DTO = DTO { _game     :: Game
               , _location :: Location
               } deriving (Show)



makeLenses ''DTO



fromData :: Data.Data -> Maybe DTO
fromData = liftA2 createDTO Data.getGame Data.getCommandLocation



createDTO :: Maybe Game -> Maybe Location -> Maybe DTO
createDTO (Just g) (Just l) = Just $ DTO g l
createDTO _ _               = Nothing



getGame :: DTO -> Game
getGame = view game



getLocation :: DTO -> Location
getLocation = view location

