-- Exercise 4

mylast []     = []
mylast [x]    = x
mylast (x:xs) = mylast xs

-- Exercise 5

myinit xs = reverse (tail (reverse xs))
