import Data.List

-- 1
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:xs) = Just x

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (x:xs) = Just xs

safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast x = Just $ last x

safeInit :: [a] -> Maybe [a]
safeInit [] = Nothing
safeInit x = Just $ init x

-- 2
splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith _ [] = []
splitWith p xs | null front = splitWith p backTail
               | null back = [front]
               | otherwise = front : splitWith p backTail
            where (front, back) = span p xs
                  backTail = tail back

-- 3
headWords :: String -> String
headWords [] = []
headWords x = unlines $ map each (lines x)
    where each [] = []
          each s = head $ words s