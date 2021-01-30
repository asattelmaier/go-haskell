module CLI (main) where



import Data.Maybe
import Go.Game       (Game, Score, Player, createGame, play, pass, end)
import CLI.Command   (Command (ExitGame, MoveCursor, PlayStone, Pass))
import CLI.Cursor    (Cursor, createCursor, translateCursor)
import CLI.Render    (renderGame, cursorToLocation, renderEndGame, askForGridSize)
import CLI.UserInput (getGridSize, getCommand)



main :: IO ()
main = do
  putStr askForGridSize
  
  gridSize <- getGridSize

  setup gridSize



setup :: Int -> IO ()
setup gridSize = run game cursor
  where game   = createGame gridSize
        cursor = createCursor



run :: Game -> Cursor -> IO ()
run game cursor = do
  putStr $ renderGame game cursor

  command <- getCommand
  
  case command of
    ExitGame               -> return ()

    MoveCursor translation -> run game $ translateCursor cursor translation

    Pass                   -> maybe endGame runGame passGame
      where passGame = pass game
            runGame  = flip run cursor
            endGame  = terminate $ end game

    PlayStone
      | isNothing location -> run game cursor
      | otherwise          -> run playGame cursor
      where playGame = play game $ fromJust location
            location = cursorToLocation cursor



terminate :: ([Player], Score) -> IO ()
terminate (winners, score) = do
  putStr $ renderEndGame (winners, score)
  return () 
