isPlalindrome :: Eq a => [a] -> Bool
isPlalindrome [] = True
isPlalindrome x = (reverse x == x)