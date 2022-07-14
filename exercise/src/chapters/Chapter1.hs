module Chapter1
    ( 
    ) where

-- 3
product' [] = 1
product' (x:xs) = x * product' xs

-- 4
qsortDesc [] = []
qsortDesc (x:xs) = qsortDesc larger ++ [x] ++ qsortDesc smaller
    where
        larger = [a | a <- xs, a > x]
        smaller = [b | b <- xs, b <= x]

-- 5
qsortDistinct [] = []
qsortDistinct (x:xs) = qsortDistinct smaller ++ [x] ++ qsortDistinct larger
    where
        smaller = [a | a <- xs, a < x]
        larger = [b | b <- xs, b > x]