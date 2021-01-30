module CLI.UserInput
( getCommand
, getGridSize
) where



import System.Process
import System.IO
import CLI.Command (Command, createCommand)



getCommand :: IO Command
getCommand = do
  hSetBuffering stdin NoBuffering 
  clearLine
  
  userInput <- getChar
  
  let command = createCommand userInput

  clearScreen
  
  return command



getGridSize :: IO Int
getGridSize = do
  nums <- getLine
  
  clearScreen
 
  return (read nums :: Int)



clearScreen :: IO ()
clearScreen = do
  system "clear"
  return ()



clearLine :: IO ()
clearLine = putStr "\ESC[2K\ESC[0G"
