{-# LANGUAGE CPP             #-}
{-# LANGUAGE TemplateHaskell #-}



module API.REST.Server where



import qualified API.REST.Controller              as Controller
import           Control.Exception                (SomeException, try)
import qualified Data.Text                        as T
import           Snap.Core
import           Snap.Http.Server
import           Snap.Internal.Http.Server.Config
import           Snap.Snaplet
import           Snap.Snaplet.Config
import           System.IO
import           Text.Read

#ifdef DEVELOPMENT
import           Snap.Loader.Dynamic
#else
import           Snap.Loader.Static
#endif



main :: Maybe String -> IO ()
main serverPort = do
  (conf, site, cleanup) <- $(loadSnapTH [| getConf (readMaybe =<< serverPort) |]
                                        'getActions
                                        ["snaplets/heist/templates"])

  _ <- try $ httpServe conf site :: IO (Either SomeException ())
  cleanup



getConf :: Maybe Int -> IO (Config Snap a)
getConf serverPort = do
  return defaultConfig { port = serverPort }



getActions :: Config Snap AppConfig -> IO (Snap (), IO ())
getActions conf = do
  (msgs, site, cleanup) <- runSnaplet
      (appEnvironment =<< getOther conf) Controller.create
  hPutStrLn stderr $ T.unpack msgs
  return (site, cleanup)
