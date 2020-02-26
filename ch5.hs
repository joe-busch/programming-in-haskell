-- sum [n^2 | n <- [1..100]]

grid :: Int -> Int -> [(Int,Int)]
grid m n = [(x,y) | x <- [0..m], y <- [0..n]]

square :: Int -> [(Int,Int)]
square n = grid n n

my_replicate :: Int -> a -> [a]
my_replicate n x = [x | _ <- [1..n]]

pyths :: Int -> [(Int,Int,Int)]
pyths n = [(x,y,z) | x <- [1..n], y <- [1..n], z <- [1..n], x^2 + y^2 == z^2]

factors :: Int -> [Int]
factors n = [m | m <- [1..n], n `mod` m == 0]

perfects :: Int -> [Int]
perfects n = [m | m <- [1..n], sum (factors m) == 2*m]

-- [(x,y) | x <- [1,2], y <- [z | z <- [3,4]]]

find :: Eq a => a -> [(a,b)] -> [b]
find k t = [v | (k',v) <- t, k == k']

positions :: Eq a => a -> [a] -> [Int]
positions x xs = find x (zip xs [0..])

scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys = sum [x*y | (x,y) <- zip xs ys]
