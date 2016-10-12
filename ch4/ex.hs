import Data.List
import Data.Chart (digitToInt, isDigit)

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


-- 4
-- foldl的step函数是a->b->a，a是初始数据类型，b是数组元素
-- foldr的step函数则是a->b->b， a是数组元素类型, b是目标元素
asInt_fold :: String -> Int
asInt_fold "" = 0
asInt_fold ('-': xs) = (-1) * digitToInt xs
asInt_fold (x:xs) = foldl' step 0 (x:xs)
    where step acc x = acc * 10 + digitToInt x

-- 5
type ErrorMessage = String
asInt_either :: String -> Either Int ErrorMessage
asInt_either "" = error "no input digit"
asInt_either ('-':xs) = (-1) * digitToInt xs
asInt_either (x:xs) = foldl' step 0 (x:xs)
    where step acc x | isDigit x = acc * 10 + digitToInt x
                     | otherwise = error "not digit"

-- 6
concat':: [[a]] -> [a]
concat' xs = foldr (++) [] xs

-- 7
takeWhile':: (a -> Bool) -> [a]
takeWhile' _ [] = []
takeWhile' f (x:xs) | f x = x : takeWhile' f xs
                    | otherwise = []

takeWhile'' :: (a -> Bool) -> [a]
takeWhile'' f xs = foldr step [] xs
    where step x taked | f x = x : taked
                       | otherwise = []

-- 8
groupBy' :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy' f xs = foldr step [] xs
    where step x grouped =  