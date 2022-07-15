module Chapter5
    ( 
    ) where

import Data.Char

-- 1
sumOfSquares :: Integer
sumOfSquares = sum [x^2 | x <- [1..100]]

-- 2
replicate' :: Int -> a -> [a]
replicate' n e = [e | _ <- [1..n]]

-- 3
pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x, y, z) | x <- [1..n], y <- [1..n], z <- [1..n], x^2 + y^2 == z^2]

-- 4
factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

perfects :: Int -> [Int]
perfects n = [x | x <- [1..n], x == sum (factors x) - x ]

-- 5
asIs = [(x, y) | x <- [1, 2, 3], y <- [4, 5, 6]]
toBe = concat [[(x, y) | y <- [4, 5, 6]] | x <- [1, 2, 3]]

-- 6
find :: Eq a => a -> [(a, b)] -> [b]
find k t = [v | (k', v) <- t, k == k']

positions :: Eq a => a -> [a] -> [Int]
positions x xs = find x (zip xs [0..])

-- 7
scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys = sum [x * y | (x, y) <- zip xs ys]

-- 8
lowerlet2int :: Char -> Int
lowerlet2int c = ord c - ord 'a'
upperlet2int :: Char -> Int
upperlet2int c = ord c - ord 'A'

int2lowerlet :: Int -> Char
int2lowerlet n = chr (ord 'a' + n)
int2upperlet :: Int -> Char
int2upperlet n = chr (ord 'A' + n)

shift :: Int -> Char -> Char
shift n c | isLower c = int2lowerlet ((lowerlet2int c + n) `mod` 26)
          | otherwise = int2upperlet ((upperlet2int c + n) `mod` 26)

encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]


