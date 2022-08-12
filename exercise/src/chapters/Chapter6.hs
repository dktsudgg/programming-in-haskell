module Chapter6
    ( 
    ) where

-- 1
power :: Integral a => a -> a -> a
power 0 0 = 1
power 0 _ = 0
power _ 0 = 1
power n1 n2 = n1 * power n1 (n2 - 1)

-- 3 - 1
and' :: [Bool] -> Bool
and' [] = True
and' (b:bs) = b && and' bs

-- 3 - 2
concat' :: [[a]] -> [a]
concat' [] = []
concat' (xs:xss) =  xs ++ concat' xss

-- 3 - 3
replicate' :: Integral a => a -> b -> [b]
replicate' 0 x = []
replicate' n x = x : replicate' (n-1) x

-- 3 - 4
(!!!) :: [a] -> Int -> a
(!!!) [] _ = error "List is empty"
(!!!) (x:_) 0 = x
(!!!) (x:xs) n
    | 0 <= n && n < length (x:xs) = (!!!) xs (n - 1)
    | otherwise = error "Wrong index"

-- 3 - 5
elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' x (y:ys)
    | x == y = True
    | otherwise = elem' x ys

-- 4
merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
    | x < y = x : merge xs (y:ys)
    | otherwise = y : merge (x:xs) ys

-- 5
halve :: [a] -> ([a], [a])
halve xs = splitAt (length xs `div` 2) xs

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort [x, y]
    | x < y = x : [y]
    | otherwise = y : [x]
msort xs = merge (msort flist) (msort blist)
  where (flist, blist) = halve xs

-- 6 - 1
sum' :: Num a => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

-- 6 - 2
take' :: Int -> [a] -> [a]
take' 0 _ = []
take' _ [] = []
take' n (x:xs)
    | 0 <= n =  x : take' (n - 1) xs
    | otherwise = []

-- 6 - 3
last' :: [a] -> a
last' [] = error "Empty list"
last' [x] = x
last' (x:xs) = last' xs
