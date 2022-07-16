module Chapter7
    ( 
    ) where

-- 1
asIsMapFilter f p = \xs -> [f x | x <- xs, p x]
toBeMapFilter f p = \xs -> (map f . filter p) xs

-- 2 - all
all' :: (a -> Bool) -> [a] -> Bool
all' p = foldr (\a1 a2 -> p a1 && a2) True

-- 2 - any
any' :: (a -> Bool) -> [a] -> Bool
any' p = foldr (\a1 a2 -> p a1 || a2) False

-- 2 - takeWhile
takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' p [] = []
takeWhile' p (x:xs)
    | p x = x : takeWhile' p xs
    | otherwise = []

-- 2 - dropWhile
dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' p [] = []
dropWhile' p (x:xs)
    | p x = dropWhile' p xs
    | otherwise = x:xs

-- 3
map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\a1 a2 -> f a1 : a2) []

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\a1 a2 -> if p a1 then a1 : a2 else a2) []

-- 4
dec2int :: [Int] -> Int
dec2int = foldl (\a1 a2 -> a1 * 10 + a2) 0

