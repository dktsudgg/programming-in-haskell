module Chapter7
    ( 
    ) where
import Data.Char

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

-- 6
curry' :: ((a, b) -> c) -> a -> b -> c
curry' f = \x y -> f (x, y)

uncurry' :: (a -> b -> c) -> (a, b) -> c
uncurry' f = \(x, y) -> f x y

-- 7
unfold p h t x
    | p x = []
    | otherwise = h x : unfold p h t (t x)

type Bit = Int

chop8' :: [Bit] -> [[Bit]]
chop8' = unfold (==[]) (take 8) (drop 8)

map'' :: (a -> b) -> [a] -> [b]
map'' f = unfold null (f . head) tail

iterate' :: (a -> a) -> a -> [a]
iterate' = unfold (const False) id

-- 8
bin2int :: [Bit] -> Int
bin2int = foldr (\x y -> x + 2 * y) 0

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

encode :: String -> [Bit]
encode = concat . map (make8 . int2bin . ord)

chop8 :: [Bit] -> [[Bit]]
chop8 [] = []
chop8 bits = take 8 bits : chop8 (drop 8 bits)

decode :: [Bit] -> String
decode = map (chr . bin2int) . chop8

transmit :: String -> String
transmit = decode . channel . encode
channel :: [Bit] -> [Bit]
channel =id

makeParityBit :: [Bit] -> Bit
makeParityBit bits = let bitCnt = sum bits in bitCnt `mod` 2

make8WithParityBit :: [Bit] -> [Bit]
make8WithParityBit bits = make8 bits ++ [makeParityBit bits]

chop8WithParityBit :: [Bit] -> [[Bit]]
chop8WithParityBit [] = []
chop8WithParityBit bits
    | parityBit == bits !! 8 = take 8 bits : chop8WithParityBit (drop 9 bits)
    | otherwise = error "Parity bit error"
    where parityBit = (makeParityBit . take 8) bits

encodeWithParityBit :: String -> [Bit]
encodeWithParityBit = concat . map (make8WithParityBit . int2bin . ord)

decodeWithParityBit :: [Bit] -> String
decodeWithParityBit = map (chr . bin2int) . chop8WithParityBit