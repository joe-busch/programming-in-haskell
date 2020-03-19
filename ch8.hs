-- 1

data Nat = Zero | Succ Nat
  deriving Show

add :: Nat -> Nat -> Nat
add Zero n     = n
add (Succ m) n = Succ (add m n)

mult :: Nat -> Nat -> Nat
mult Zero n     = Zero
mult (Succ m) n = add n (mult m n)

-- 2

{- data Tree a = Leaf a | Node (Tree a) a (Tree a)

occurs :: Ord a => a -> Tree a -> Bool
occurs x (Leaf y)     = compare x y == EQ
occurs x (Node l y r) = case compare x y of
                          LT -> occurs x l
                          EQ -> True
                          GT -> occurs x r
-}

-- 3

data Tree a = Leaf a | Node (Tree a) (Tree a)

numLeaves :: Tree a -> Int
numLeaves Leaf x = 1
numLeaves Node l r = numLeaves l + numLeaves r

balanced :: Tree a -> Bool
balanced Leaf a   = True
balanced Node l r = abs(numLeaves l - numLeaves r) <= 1 && balanced l && balanced r

-- 4

halve :: [a] -> ([a],[a])
halve xs = (take half xs, drop half xs)
  where half = length xs `div` 2

balance :: [a] -> Tree
balance [x] = Leaf x
balance xs  = Node (balance ls) (balance rs)
                where (ls,rs) = halve xs

-- 5
