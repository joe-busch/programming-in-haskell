myadd :: Int -> Int -> Int -> Int
myadd x y z = x + y + z

mycopy a = (a,a)

myap f x = f x

second xs = head (tail xs)

swap (x,y) = (y,x)

pair x y = (x,y)

double x = x*2

palidrome xs = reverse xs == xs

twice f x = f (f x)
