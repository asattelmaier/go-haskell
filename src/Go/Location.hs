module Go.Location
( Location (Location)
, getX
, getY
) where



data Location     = Location Int Int deriving (Show, Eq)



getX :: Location -> Int
getX (Location x y) = x



getY :: Location -> Int
getY (Location x y) = y

