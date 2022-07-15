module Chapter3
    ( 
    ) where

-- 1
halve :: [a] -> ([a], [a])
halve xs = (take halfCnt xs, drop halfCnt xs)
    where halfCnt = length xs `div` 2

-- 2-a
safetail :: [a] -> [a]
safetail xs = if null xs then error "Cannot use empty list for this function (2-a)"
              else tail xs

-- 2-b
safetail' :: [a] -> [a]
safetail' xs | null xs = error "Cannot use empty list for this function (2-b)"
             | otherwise = tail xs

-- 2-c
safetail'' :: [a] -> [a]
safetail'' [] = error "Cannot use empty list for this function (2-c)"
safetail'' (_:xs) = xs