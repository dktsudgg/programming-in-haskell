module Chapter10
    (
    ) where

data Nat = Zero | Succ Nat

nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ n) = 1 + nat2int n

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ (int2nat (n - 1))

add :: Nat -> Nat -> Nat
-- add m n = int2nat (nat2int m + nat2int n)
add Zero n = n
add (Succ m) n = Succ (add m n)

-- 1
mult :: Nat -> Nat -> Nat
mult Zero _ = Zero
mult _ Zero = Zero
mult m n = add m (mult m nMinus1)
  where
    nMinus1 = int2nat $ nat2int n - 1

-- 2
data Tree = Leaf Int | Node Tree Int Tree

t :: Tree
t = Node (Node (Leaf 1) 3 (Leaf 4)) 5 (Node (Leaf 6) 7 (Node (Leaf 8) 9 (Node (Leaf 10) 11 (Leaf 12))))

occurs :: Int -> Tree -> Bool
occurs m (Leaf n) = m == n
-- occurs m (Node l n r) = m == n || occurs m l || occurs m r
-- occurs m (Node l n r)
--     | m == n = True
--     | m < n = occurs m l
--     | otherwise = occurs m r
occurs m (Node l n r) = case compare m n of
    EQ -> True
    LT -> occurs m l
    GT -> occurs m r

-- 3
data BinaryTree = BinaryLeaf Int | BinaryNode BinaryTree BinaryTree
    deriving Show
cntOfLeafIn :: BinaryTree -> Int
cntOfLeafIn (BinaryLeaf _) = 1
cntOfLeafIn (BinaryNode l r) = cntOfLeafIn l + cntOfLeafIn r

isBalanced :: BinaryTree -> Bool
isBalanced (BinaryLeaf _) = True
isBalanced (BinaryNode l r) = diff <= 1
  where diff = abs $ cntOfLeafIn l - cntOfLeafIn r

-- 4
balance :: [Int] -> BinaryTree
balance [x] = BinaryLeaf x
balance xs = BinaryNode (balance lList) (balance rList)
  where (lList, rList) = divide xs

divide :: [Int] -> ([Int], [Int])
divide xs = splitAt half xs
  where half = length xs `div` 2
