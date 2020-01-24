-- Exercise 3

my_product []     = 1
my_product (n:ns) = n * my_product ns

-- Exercise 4

reverse_qsort [] = []
reverse_qsort (x:xs) = reverse_qsort larger ++ [x] ++ reverse_qsort smaller
  where
    larger   = [a | a <- xs, a >= x]
    smaller  = [b | b <- xs, b < x]
