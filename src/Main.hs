module Main where



import qualified API.JSON           (main)
import qualified API.REST.Server    (main)
import qualified CLI                (main)
import           System.Environment
import           System.Exit



main :: IO ()
main = getArgs >>= parse



parse :: [String] -> IO ()
parse ["h"]          = usage >> exit
parse ["cli"]        = CLI.main
parse ["json", json] = API.JSON.main json
parse ["rest", port] = API.REST.Server.main $ Just port
parse ["rest"]       = API.REST.Server.main Nothing



usage :: IO ()
usage = putStr $
  "Usage:\n" ++
  "  go-haskell command [command options]\n\n" ++
  "Version:\n" ++
  "  0.0.0.1\n\n" ++
  "Commands:\n" ++
  "  cli         Runs Go Haskell as CLI Game.\n" ++
  "  h           Print this information.\n" ++
  "  json JSON   Go Haskell JSON API.\n" ++
    -- TODO: Make Address configurable.
  "  rest PORT   Runs Go Haskell REST API on \"0.0.0.0\"\n" ++
  "               Port to listen on, default 8000.\n"



exit :: IO ()
exit = exitSuccess

