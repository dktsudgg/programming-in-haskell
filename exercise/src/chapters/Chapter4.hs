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

-- 3-1
(|||) :: Bool -> Bool -> Bool
True ||| True = True
True ||| False = True
False ||| True = True
False ||| False = False

-- 3-2
(||||) :: Bool -> Bool -> Bool
False |||| False = False
_ |||| _ = True

-- 3-3
(|||||) :: Bool -> Bool -> Bool
False ||||| b = b
True ||||| _ = True

-- 3-4
(||||||) :: Bool -> Bool -> Bool
b |||||| c | b = True
          | c = True
          | otherwise = False

