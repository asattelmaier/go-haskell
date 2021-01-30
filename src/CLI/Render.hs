{-# LANGUAGE NamedFieldPuns #-}

module CLI.Render
( renderGame
, cursorToLocation
, renderEndGame
, askForGridSize
) where



import Go.Board
import Go.Game      (Game (Game, positions), Score, Player)
import CLI.Cursor   (Cursor (Cursor))


cursorRepresentation :: Char
cursorRepresentation = '█'

empty :: Char
empty = '+'

blackStone :: Char
blackStone = 'X'

whiteStone :: Char
whiteStone = 'O'

gutter :: Char
gutter = ' '

horizontalGutter :: Char
horizontalGutter = '-'

verticalGutter :: Char
verticalGutter = '|'

verticalGutterSpace :: Int
verticalGutterSpace = 3

horizontalGutterSpace :: Int
horizontalGutterSpace = 1



renderGame :: Game -> Cursor -> String
renderGame game cursor = renderCursor game cursor $ renderBoard game



cursorToLocation :: Cursor -> Maybe Location
cursorToLocation (Cursor x y)
  | isOverIntersection = Just (Location locationX locationY)
  | otherwise          = Nothing
  where locationX           = x `div` (verticalGutterSpace + 1)
        locationY           = y `div` (horizontalGutterSpace + 1)
        isOverIntersection  = isXOverIntersection && isYOverIntersection
        isXOverIntersection = x `mod` (verticalGutterSpace + 1) == 0
        isYOverIntersection = y `mod` (horizontalGutterSpace + 1) == 0



renderCursor :: Game -> Cursor -> String -> String
renderCursor Game {positions = currentPosition:lastPositions} cursor renderedBoard =
  replaceChar renderedBoard index cursorRepresentation
  where index = getCursorLocationIndex cursor currentPosition



getCursorLocationIndex :: Cursor -> Board -> Int
getCursorLocationIndex (Cursor x y) board = x + y * (horizontalLines - 1) * 4 + y * 2
  where horizontalLines = length $ head board



renderBoard :: Game -> String
renderBoard Game {positions} = removeLastLine $ concatMap renderGrid $ head positions
  where removeLastLine      = take $ horizontalLines * horizontalLineWidth * (1 + horizontalGutterSpace) - lastLine
        horizontalLines     = length $ head positions
        horizontalLineWidth = (verticalGutterSpace + 1) * (horizontalLines - 1) + 2
        lastLine            = horizontalGutterSpace * horizontalLineWidth



renderGrid :: [Intersection] -> String
renderGrid intersections = unlines $ renderHorizontalLine intersections : renderVerticalLines intersections



renderHorizontalLine :: [Intersection] -> String
renderHorizontalLine = renderLine . map (renderHorizontalGutter . renderIntersection)



renderVerticalLines :: [Intersection] -> [String]
renderVerticalLines = replicate horizontalGutterSpace . renderLine . map renderVerticalLine 



renderLine :: [String] -> String
renderLine a = take ((length a - 1) * (verticalGutterSpace + 1) + 1) $ concat a



renderHorizontalGutter :: Char -> String
renderHorizontalGutter = flip (:) (replicate verticalGutterSpace horizontalGutter)



renderVerticalLine :: Intersection -> String
renderVerticalLine _ = verticalGutter:replicate verticalGutterSpace gutter



renderIntersection :: Intersection -> Char
renderIntersection (Intersection location Empty)         = empty
renderIntersection (Intersection location (Stone Black)) = blackStone
renderIntersection (Intersection location (Stone White)) = whiteStone



renderEndGame :: ([Player], Score) -> String
renderEndGame (winners, score)
  | isDrawn   = renderScore ++ renderDrawn
  | otherwise = renderScore ++ renderWinner 
  where isDrawn      = length winners == 2
        renderScore  = "\n" ++ "Score: " ++ show score
        renderWinner = "\n" ++ "Winner: " ++ show (head winners) ++ "\n"
        renderDrawn  = "\n" ++ "Drawn" ++ "\n"


-- General Utils

replaceChar :: String -> Int -> Char -> String
replaceChar [] _ _ = []
replaceChar (_:stringTail) 0 char = char:stringTail
replaceChar (stringHead:stringTail) index char =
  if index < 0
    then stringHead:stringTail
    else stringHead:replaceChar stringTail (index - 1) char


askForGridSize :: String
askForGridSize = "What grid size do you want to play with?\n"
