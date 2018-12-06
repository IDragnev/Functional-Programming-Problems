module Problems where

import Prelude hiding (iterate, splitAt, cycle, zipWith, zip, unzip, append, sum, product, map, take, reverse, all, any, less, minimum, maximum, foldl, foldr, fst, snd)

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

append to what = foldr (:) what to

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
--isPrime n = (sumDivisors n) == n + 1
isPrime n = null [ m | m<-[2..sqn], n `mod` m == 0 ]
 where sqn = floor (sqrt (fromIntegral n))

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

pythTriples = [ (a, b, c) | c<-[5..], b<-[1..c], a<-[1..b], a^2 + b^2 == c^2 ]

primes' = sieve [2..]
 where sieve (h:t) = h : sieve (dropMultiplesOf h t)
       dropMultiplesOf x list = filter  (\y -> y `mod` x /= 0)  list

maxRepeated [] = 0
maxRepeated (h:t) = maximum (helper [] 1 h t)
 where
  helper lens count _ [] = (count:lens)
  helper lens count last (h:t)
   | h == last = helper lens (count + 1) h t
   | otherwise = helper (count : lens) 1 h t

compress :: Eq t => [t] -> [t]
compress [] = []
--compress (h:t) = helper [h] h t
-- where
--   helper result _ [] = result
--   helper result last (h:t) 
--    | h == last = helper result last t
--   | otherwise = helper (result ++ [h]) h t
compress [x] = [x]
compress (x:y:t) 
 | x == y = compress (y:t)
 | otherwise = x : compress (y:t)

zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith f [] _ = []
zipWith f _ [] = []
zipWith f (x:xs) (y:ys) = (f x y) : zipWith f xs ys

zip = zipWith (,)

unzip :: [(a, b)] -> ([a], [b])
unzip list = foldl (\(lhs, rhs) (x,y) -> (x:lhs, y:rhs)) ([],[]) list 

quickSort [] = []
quickSort (h:t) = smaller ++ (h:greater)
 where 
   smaller = quickSort (filter (<h) t)
   greater = quickSort (filter (>=h) t)

cycle [] = []
--cycle = list ++ cycle list
cycle list = helper list
 where helper (h:t)
        | null t = h : helper list
        | otherwise = h : helper t

duplicate list = reverse (foldl (\ result x -> x:x:result) [] list)

merge :: Ord a => [a] -> [a] -> [a]
merge [] rhs = rhs
merge lhs [] = lhs
merge (x:xs) (y:ys) 
 | x <= y = x : merge xs (y:ys)
 | otherwise = y : merge (x:xs) ys

splitAt _ [] = ([], [])
splitAt n list = (head, tail)
 where 
  head = take n list
  tail = drop n list

mergeSort [] = []
mergeSort [x] = [x]
mergeSort list = 
 let 
   middle = (length list) `div` 2
   split = splitAt middle list
   lhs = mergeSort (fst split)
   rhs = mergeSort (snd split)
  in merge lhs rhs

iterate f x = let y = f x in x : iterate f y
