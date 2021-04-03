module API.WebSocket.Controller
( handle
) where



import qualified API.WebSocket.Service   as Service
import qualified Data.Aeson              as JSON
import           Data.Text.Lazy          (Text)
import           Data.Text.Lazy.Encoding (decodeUtf8, encodeUtf8)



handle :: Text -> Text
handle clientData = do
  let inputDTO = JSON.decode (encodeUtf8 clientData)

  decodeUtf8 $ JSON.encode $ Service.createGame inputDTO
