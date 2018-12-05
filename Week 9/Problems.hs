module Problems where

import Prelude hiding (sum, product, map, take, reverse, all, any, less, minimum, maximum, foldl, foldr, fst, snd)

type Point = (Double, Double)

fst (x, _) = x
snd (_, y) = y 

distance :: Point -> Point -> Double
distance p1 p2 = sqrt (du^2 + dv^2)
  where du = fst p1 - fst p2
        dv = snd p1 - snd p2

foldl op result [] = result
foldl op result (h:t) = foldl op (op result h) t 

foldr op nv [] = nv
foldr op nv (h:t) = op h (foldr op nv t)

map op [] = []
map op (h:t) = (op h) : map op t

minimum (h:t) = foldl min h t

maximum (h:t) = foldl max h t

any p [] = False
any p (h:t) = p h || any p t

all p list = not (any (\ x -> not (p x)) list)

reverse [] = []
reverse list = foldl (\ result x -> (x : result)) [] list 

sum = foldl (+) 0

product = foldl (*) 1

sumDivisors 0 = 0
sumDivisors n = sum [ x | x<-[1..n], n `mod` x == 0]

isPrime 0 = False
isPrime 1 = False
isPrime 2 = True
isPrime n = (sumDivisors n) == n + 1

descartes [] _ = []
descartes _ [] = []
descartes lhs rhs = [ (x, y) | x<-lhs, y<-rhs ]

take _ [] = []
take 0 _  = []
take n (h:t) = h : take (n - 1) t

maxDistance list = 
  let pairs = descartes list list 
      distances = map (\ pair -> distance (fst pair) (snd pair)) pairs
  in maximum distances

member x = foldl (\ result y -> result || y == x) False

makeSet [] = []
makeSet (h:t) 
 | member h t = makeSet t
 | otherwise = h : makeSet t

primes = [ x | x<-[2..], isPrime x ]

nats = [0..]

natPairs = [ (x, z - x) | z<-nats, x<-[0..z] ]

countOccurances x [] = 0
countOccurances x (h:t) 
 | x == h = 1 + (countOccurances x t)
 | otherwise = countOccurances x t

histogram [] = []
histogram list = foldl (\ result x -> (makePair x) : result)  [] (makeSet list)
 where makePair x = (x, countOccurances x list)
