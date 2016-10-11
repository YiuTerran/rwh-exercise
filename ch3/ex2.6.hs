import Data.List (sortBy)

cmp :: [a] -> [b] -> Ordering
cmp x y
    | lx < ly = LT
    | lx == ly = EQ
    | otherwise = GT
  where lx = length x
        ly = length y

sortByLength :: [[a]] -> [[a]]
sortByLength [] = []
sortByLength x = sortBy cmp x