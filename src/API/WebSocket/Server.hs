{-# LANGUAGE OverloadedStrings #-}



module API.WebSocket.Server where



import qualified API.WebSocket.Controller         as Controller (handle)
import           Control.Monad                    (forever)
import           Data.ByteString                  (ByteString)
import qualified Data.ByteString.Char8            as B (pack)
import           Data.Text.Lazy                   (Text)
import qualified Network.WebSockets               as WS (Connection, ServerApp,
                                                         acceptRequest,
                                                         receiveData,
                                                         sendTextData,
                                                         withPingThread)
import qualified Network.WebSockets.Snap          as WS (runWebSocketsSnap)
import           Snap.Core                        (Snap)
import qualified Snap.Core                        as Snap (route)
import           Snap.Http.Server                 (Config)
import qualified Snap.Http.Server                 as Snap (defaultConfig,
                                                           httpServe)
import qualified Snap.Internal.Http.Server.Config as Snap (bind, port)
import           Text.Read                        (readMaybe)



type Client = WS.Connection



main :: Maybe String -> Maybe String -> IO ()
main host port = do runServer $ getConfig (readHost host) (readMaybe =<< port)



readHost :: Maybe String -> Maybe ByteString
readHost = fmap B.pack



getConfig :: Maybe ByteString -> Maybe Int -> Config Snap a
getConfig host port = Snap.defaultConfig { Snap.port = port, Snap.bind = host }



runServer :: Config Snap a -> IO ()
runServer config = do Snap.httpServe config routes



routes :: Snap ()
routes = Snap.route [("", WS.runWebSocketsSnap app)]



app :: WS.ServerApp
app pending = do
  client <- WS.acceptRequest pending

  putStrLn "New Client"

  WS.withPingThread client 30 (return ()) $ do
    communicate client



communicate :: Client -> IO ()
communicate client = forever $ do
  clientData <- WS.receiveData client :: IO Text

  WS.sendTextData client $ Controller.handle clientData

