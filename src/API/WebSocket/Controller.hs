module API.WebSocket.Controller
( handle
) where


import           API.WebSocket.Input.Command
import qualified API.WebSocket.Input.CreateGameDTO as CreateGameDTO
import           API.WebSocket.Input.Data
import qualified API.WebSocket.Input.PassDTO       as PassDTO
import qualified API.WebSocket.Input.PlayStoneDTO  as PlayStoneDTO
import qualified API.WebSocket.Service             as Service
import qualified Data.Aeson                        as JSON
import           Data.Text                         (pack)
import           Data.Text.Lazy                    (Text)
import           Data.Text.Lazy.Encoding           (decodeUtf8, encodeUtf8)



handle :: Text -> Text
handle rawData = do


  case JSON.decode . encodeUtf8 $ rawData of

    Nothing  -> respondError "No valid data provided"

    Just inputData ->


      case getCommandName inputData of

        CreateGame -> createGame inputData

        PlayStone  -> playStone inputData

        Pass       -> pass inputData



createGame :: Data -> Text
createGame = respond . fmap Service.createGame . CreateGameDTO.fromData



playStone :: Data -> Text
playStone = respond . fmap Service.playStone . PlayStoneDTO.fromData



pass :: Data -> Text
pass = respond . fmap Service.pass . PassDTO.fromData



respond :: JSON.ToJSON a => a -> Text
respond = decodeUtf8 . JSON.encode



respondError :: String -> Text
respondError = respond . JSON.String . pack . (++) "Error: "

