module Problems where

import Prelude hiding (gcd, fst, snd)

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

gcd :: Int -> Int -> Int
gcd 0 0 = error "No gcd for 0 and 0"
gcd 0 x = x
gcd y 0 = y
gcd x y 
 | x > y = gcd (x - y) y
 | otherwise = gcd x (y - x)

power :: Int -> Int -> Int
power x 0 = 1
power x y
 | odd y = x * power x (y - 1)
 | otherwise = m * m 
 where m = power x (div y 2)

ackermann :: Int -> Int -> Int
ackermann 0 n = n + 1
ackermann m n 
 | m < 0 = error "m cannot be negative" 
 | n == 0 = ackermann (m - 1) 1
 | otherwise = ackermann  (m - 1) (ackermann m n - 1) 

distance (x1, y1) (x2, y2) = sqrt (dx^2 + dy^2)
 where dx = x2 - x1
       dy = y2 - y1

modulus = distance (0, 0)        

fst :: (t, t) -> t
fst (x, _) = x

snd :: (t, t) -> t
snd (_, y) = y

type Point = (Double, Double) 
type PointOp = Point -> Point

complOp op p1 p2 = (op (fst p1) (fst p2), op (snd p1) (snd p2))

complAdd :: Point -> Point -> Point
complAdd x y = complOp (+) x y

complSub :: Point -> Point -> Point
complSub x y = complOp (-) x y
