module Chapter2
    ( 
    ) where

-- 3
n = a `div` length xs
    where
        a = 10
        xs = [1, 2, 3, 4, 5]

-- 4
last' xs = head (reverse xs)

last'' xs = xs !! (length xs - 1)

-- 5
init' xs = reverse (drop 1 (reverse xs))

init'' xs = take (length xs - 1) xs