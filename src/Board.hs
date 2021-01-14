module Board
( Board
, createBoard
) where

import Stone (Stone (Stone))

data Point = Empty | Stone deriving (Show)
type Board = [[Point]]

createBoard :: Int -> Int -> Board
createBoard rows cols = createPoints rows cols

createPoints :: Int -> Int -> [[Point]]
createPoints 0    cols = []
createPoints rows cols = (createPoints (rows - 1) cols) ++ [createRow cols]

createRow :: Int -> [Point]
createRow 0    = []
createRow cols = (createRow (cols - 1)) ++ [Empty]

