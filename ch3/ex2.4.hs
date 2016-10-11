plalindrome :: [a] -> [a]
plalindrome [] = []
plalindrome x = x ++ reverse x