module API.WebSocket.Controller
( handle
) where


import           API.WebSocket.Input.Command
import qualified API.WebSocket.Input.CreateDTO as CreateDTO
import           API.WebSocket.Input.Data
import qualified API.WebSocket.Input.PassDTO   as PassDTO
import qualified API.WebSocket.Input.PlayDTO   as PlayDTO
import qualified API.WebSocket.Service         as Service
import qualified Data.Aeson                    as JSON
import           Data.Text                     (pack)
import           Data.Text.Lazy                (Text)
import           Data.Text.Lazy.Encoding       (decodeUtf8, encodeUtf8)



handle :: Text -> Text
handle rawData = do


  case JSON.decode . encodeUtf8 $ rawData of

    Nothing  -> respondError "No valid data provided"

    Just inputData ->


      case getCommandName inputData of

        Create -> create inputData

        Play   -> play inputData

        Pass   -> pass inputData



create :: Data -> Text
create = respond . fmap Service.create . CreateDTO.fromData



play :: Data -> Text
play = respond . fmap Service.play . PlayDTO.fromData



pass :: Data -> Text
pass = respond . fmap Service.pass . PassDTO.fromData



respond :: JSON.ToJSON a => a -> Text
respond = decodeUtf8 . JSON.encode



respondError :: String -> Text
respondError = respond . JSON.String . pack . (++) "Error: "

