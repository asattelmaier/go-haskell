import Data.Char
import Data.Maybe
import Foreign.C.Types
import System.Process
import Command         (Command (ExitGame, MoveCursor, PlaceStone), createCommand)
import Game            (Game (Game, board, activePlayer, passivePlayer), createGame, placeStone)
import Render          (render, cursorToPosition)
import Cursor          (Cursor, createCursor, translateCursor)

-- TODO: Board should be configurable by the user

main :: IO ()
main = do

  run game cursor

  where game   = createGame 9
        cursor = createCursor



run :: Game -> Cursor -> IO ()
run game cursor = do
--- TODO: This will work only on Windows, find a general solution 
  system "cls"
  putStr $ render game cursor
  userInput <- getHiddenChar

  let command = createCommand userInput
  
  case command of
    ExitGame          -> return ()
    MoveCursor vector -> run game (translateCursor cursor vector)
    PlaceStone
      | isNothing position -> run game cursor
      | otherwise          -> run (placeStone game (fromJust position)) cursor
      where position = cursorToPosition cursor




{-# LANGUAGE ForeignFunctionInterface #-}
getHiddenChar = fmap (chr.fromEnum) c_getch
foreign import ccall unsafe "conio.h getch"
  c_getch :: IO CInt
