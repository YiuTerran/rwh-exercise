data Direction = LEFT | RIGHT | STRIGHT
    deriving (Show, Eq)

data Point = Point {
    x :: Double,
    y :: Double
} deriving (Eq, Show)

getDirection a b c 
    | sign > 0 = RIGHT
    | sign < 0 = LEFT
    | otherwise STRIGHT
  where sign = (x b - x a) * (y c - y a) - (y b - y a) * (x c - x a)
getDirection (a : b : c : d) = getDirection a b c : getDirection (b:c:d)
getDirection _ = []

