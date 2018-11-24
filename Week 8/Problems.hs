module Problems where
fib 0 = 0
fib 1 = 1
fib n = fib(n - 1) + fib(n - 2) 

fibo n
 | n == 0 = 0
 | n == 1 = 1
 | otherwise = fibo(n - 1) + fibo(n - 2) 

countRoots a b c
 | a == 0 = "Linear equation"
 | d == 0 = "One root"
 | d > 0 = "Two roots"
 | otherwise = "No real roots"
 where d = b^2 - 4*a*c

gcd 0 x = x
gcd y 0 = y
gcd x y 
 | x > y = gcd (x - y) y
 | otherwise gcd x (y - x)

power :: Int -> Int
power x y
 | y == 0 = 1
 | even y = let m = power x (div y 2) in m * m
 | otherwise = x * power x (y - 1)
