module Main where



import qualified API.JSON.Client      (main)
import qualified API.REST.Server      (main)
import qualified API.WebSocket.Server (main)
import qualified CLI.Client           (main)
import           System.Environment   (getArgs)
import           System.Exit          (exitSuccess)



main :: IO ()
main = getArgs >>= parse



parse :: [String] -> IO ()
parse ["h"]                  = usage >> exit
parse ["cli"]                = CLI.Client.main
parse ["json", json]         = API.JSON.Client.main json
parse ["rest", port]         = API.REST.Server.main $ Just port
parse ["rest"]               = API.REST.Server.main Nothing
parse ["socket", host, port] = API.WebSocket.Server.main (Just host) (Just port)
parse ["socket"]             = API.WebSocket.Server.main Nothing Nothing



usage :: IO ()
usage =
  putStr $
    "Usage:\n"
      ++ "  go-haskell command [command options]\n\n"
      ++ "Version:\n"
      ++ "  0.0.0.1\n\n"
      ++ "Commands:\n"
      ++ "  cli                Runs Go Haskell as CLI Game.\n"
      ++ "  h                  Print this information.\n"
      ++ "  json   JSON        Go Haskell JSON API.\n"
      ++ "  rest   PORT        Go Haskell REST API\n"
      ++ "                     Host: \"ws://localhost\"\n"
      ++ "                     Default Port: 8000\n"
      ++ "  socket HOST PORT   Go Haskell WebSocket API\n"
      ++ "                     Default Host: \"ws://localhost\"\n"
      ++ "                     Default Port: 8000"



exit :: IO ()
exit = exitSuccess
