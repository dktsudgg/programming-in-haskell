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