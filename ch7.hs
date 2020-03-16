-- 1

mycomp :: (a -> a) -> (a -> Bool) -> [a] -> [a]
mycomp f p = map f . filter p

-- 2

myall :: (a -> Bool) -> [a] -> Bool
myall p []     = True
myall p (x:xs) | p x       = myall p xs
               | otherwise = False

myany :: (a -> Bool) -> [a] -> Bool
myany p []     = False
myany p (x:xs) | p x       = True
               | otherwise = myany p xs

mytakeWhile :: (a -> Bool) -> [a] -> [a]
mytakeWhile p []                 = []
mytakeWhile p (x:xs) | p x       = x : mytakeWhile p xs
                     | otherwise = mytakeWhile p xs

mydropWhile :: (a -> Bool) -> [a] -> [a]
mydropWhile p []                 = []
mydropWhile p (x:xs) | p x       = mydropWhile p xs
                     | otherwise = x: mydropWhile p xs

-- 3

mymap :: (a -> a) -> [a] -> [a]
mymap f = foldr (\x xs -> f x : xs) []

myfilter :: (a -> Bool) -> [a] -> [a]
myfilter p = foldr (\x xs -> if p x then x : xs else xs) []

-- 4

dec2int :: [Int] -> Int
dec2int = foldl (\x y -> 10*x + y) 0

-- 5

mycurry :: ((a,b) -> c) -> (a -> b -> c)
mycurry f = \x -> \y -> f (x,y)

myuncurry :: (a -> b -> c) -> ((a,b) -> c)
myuncurry f = \(x,y) -> f x y

-- 6

unfold :: (a -> Bool) -> (a -> b) -> (a -> a) -> a -> [b]
unfold p h t x | p x = []
               | otherwise = h x : unfold p h t (t x)

type Bit = Int

mychop8 :: [Bit] -> [[Bit]]
mychop8 = unfold (== []) (take 8) (drop 8)

mymap2 :: Eq a => (a -> b) -> [a] -> [b]
mymap2 f = unfold (== []) (f . head) tail

myiterate :: (a -> a) -> a -> [a]
myiterate f = unfold (\_ -> False) id f

-- 7

count :: Eq a => a -> [a] -> Int
count x = length . filter (== x)

bit2parity :: [Bit] -> [Bit]
bit2parity bs | even (count 1 bs) = bs ++ [0]
              | otherwise         = bs ++ [1]

checkPar :: [Bit] -> [Bit]
checkPar bs | parity == 0 && even (count 1 first8) = first8
            | parity == 1 && odd  (count 1 first8) = first8
            | otherwise                            = error "Parity check failed"
                where parity = head (reverse bs)
                      first8 = (reverse . tail . reverse) bs

-- 8

channel :: [Bit] -> [Bit]
channel bs = tail bs

-- 9

altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap _ _ []     = []
altMap f g (x:xs) = f x : altMap g f xs

-- 10

luhnDouble :: Int -> Int
luhnDouble n | 2*n > 9   = 2*n - 9
             | otherwise = 2*n

luhn :: [Int] -> Bool
luhn ds | tot `mod` 10 == 0 = True
        | otherwise         = False
            where tot = sum (altMap id (luhnDouble) (reverse ds))
