lastButOne :: [a] -> Maybe a
lastButOne x | l < 2 = Nothing
             | otherwise = Just $ x !! (l - 2)
        where l = length x