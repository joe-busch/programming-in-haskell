halve :: [a] -> ([a],[a])
halve xs = (take half xs, drop half xs)
  where half = length xs `div` 2

third_a :: [a] -> a
third_a xs = head (tail (tail xs))

third_b :: [a] -> a
third_b xs = xs !! 2

third_c :: [a] -> a
third_c (_:_:x:_) = x

safetail_a :: [a] -> [a]
safetail_a xs = if null xs then [] else tail xs

safetail_b :: [a] -> [a]
safetail_b xs | null xs   = []
              | otherwise = tail xs

safetail_c :: [a] -> [a]
safetail_c []    = []
safetail_c (_:xs) = xs

myand_a :: Bool -> Bool -> Bool
myand_a p q = if p then if q then True else False else False

myand_b :: Bool -> Bool -> Bool
myand_b p q = if p then q else False

mult :: Int -> Int -> Int -> Int
mult = \a -> \b -> \c -> a * b * c

luhnDouble :: Int -> Int
luhnDouble n | 2*n > 9   = 2*n - 9
             | otherwise = 2*n

luhn :: Int -> Int -> Int -> Int -> Bool
luhn a b c d = if mod (luhnDouble a + b + luhnDouble c + d) 10 == 0 then True else False
