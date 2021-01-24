module Main where



import Data.Char
import Data.Maybe
import System.Process
import System.IO
import Go.Game                 (Game, Score, Player, createGame, play, pass, end)
import UserInterface.Command   (Command (ExitGame, MoveCursor, PlayStone, Pass), createCommand)
import UserInterface.Cursor    (Cursor, createCursor, translateCursor)
import UserInterface.Render    (render, cursorToLocation)
import UserInterface.UserInput (getUserInput)



main :: IO ()
main = run game cursor

-- TODO: Board should be configurable by the user
  where game   = createGame 9
        cursor = createCursor



run :: Game -> Cursor -> IO ()
run game cursor = do
  system "clear"

  putStr $ render game cursor
  userInput <- getUserInput

  let command = createCommand userInput
  
  case command of
    ExitGame               -> terminate 0
    MoveCursor translation -> run game $ translateCursor cursor translation
    Pass                   -> maybe (terminate $ end game) (`run` cursor) (pass game)
    PlayStone
      | isNothing location -> run game cursor
      | otherwise          -> run (play game $ fromJust location) cursor
      where location = cursorToLocation cursor



-- TODO: Render Winner and Score
terminate :: Score -> IO ()
terminate score = do
  print $ show score
  return ()
