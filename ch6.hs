fac :: Int -> Int
fac n | n <= 1 = 1
      | otherwise = n * fac (n-1)

sumdown :: Int -> Int
sumdown 0 = 0
sumdown n = n + sumdown (n-1)

power :: Int -> Int -> Int
power m n | n == 0 = 1
          | otherwise = m * (power m (n - 1))

euclid :: Int -> Int -> Int
euclid m n | m == n = n
           | m < n = euclid m (n - m)
           | otherwise = euclid (m - n) n

myand :: [Bool] -> Bool
myand []     = True
myand (x:xs) = x && myand xs

myconcat :: [[a]] -> [a]
myconcat []       = []
myconcat (xs:xss) = xs ++ myconcat xss

myreplicate :: Int -> a -> [a]
myreplicate 0 _ = []
myreplicate n x = [x] ++ myreplicate (n-1) x

mybangbang :: [a] -> Int -> a
mybangbang (x:xs) 1 = x
mybangbang (x:xs) n = mybangbang xs (n-1)

myelem :: Eq a => a -> [a] -> Bool
myelem x []     = False
myelem x (y:ys) | x == y    = True
              | otherwise = myelem x ys

merge :: Ord a => [a] -> [a] -> [a]
merge xs []         = xs
merge [] ys         = ys
merge (x:xs) (y:ys) = if x < y then x : merge xs (y:ys) else y : merge (x:xs) ys

halve :: [a] -> ([a],[a])
halve xs = (take n xs,drop n xs)
  where n = length xs `div` 2

msort :: Ord a => [a] -> [a]
msort [x] = [x]
msort xs  = merge (msort (fst (halve xs))) (msort (snd (halve xs)))

mysum :: Num a => [a] -> a
mysum = foldr (+) 0

mytake :: Int -> [a] -> [a]
mytake 0 _      = []
mytake n (x:xs) = [x] ++ mytake (n-1) xs

mylast :: [a] -> a
mylast [x]    = x
mylast (x:xs) = mylast xs
