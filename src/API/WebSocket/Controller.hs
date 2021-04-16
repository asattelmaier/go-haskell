module API.WebSocket.Controller
( handle
) where


import           API.JSON.Input.Command
import qualified API.WebSocket.Input.CreateGameDTO as CreateGameDTO
import           API.WebSocket.Input.Data
import qualified API.WebSocket.Input.PlayGameDTO   as PlayGameDTO
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

        PlayStone -> playGame inputData

        NewGame   -> createGame inputData

        _         -> respondError "No valid command provided"



createGame :: Data -> Text
createGame = respond . fmap Service.createGame . CreateGameDTO.fromData 



playGame :: Data -> Text
playGame = respond . fmap Service.playGame . PlayGameDTO.fromData 



respond :: JSON.ToJSON a => a -> Text
respond = decodeUtf8 . JSON.encode



respondError :: String -> Text
respondError = respond . JSON.String . pack . (++) "Error: "

