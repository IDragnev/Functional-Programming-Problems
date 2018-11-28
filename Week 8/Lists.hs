module Lists where
 
import Prelude hiding (all, any, sorted, take, drop, map, filter, foldl, foldr, (!!), head, tail, null, length, enumFromTo, enumFromThenTo, (++), reverse)

head :: [a] -> a
head (h:_) = h

tail :: [a] -> [a]
tail (_:t) = t

null :: [a] -> Bool
null [] = True
null _ = False

length :: [a] -> Int
length [] = 0
length (h:t) = 1 + length t

enumFromTo :: Int -> Int -> [Int]
enumFromTo from to
 | from > to = []
 | otherwise = from : enumFromTo (from + 1) to

enumFromStepTo :: Int -> Int -> Int -> [Int]
enumFromStepTo from step to
 | from > to = []
 | otherwise = from : enumFromStepTo (from + step) step to

(++) :: [a] -> [a] -> [a]
[] ++ rhs = rhs
(h:t) ++ rhs = h : t ++ rhs

reverse :: [a] -> [a]
reverse [] = []
reverse (h:t) = reverse t ++ [h]

(!!) :: Int -> [a] -> a
(!!) _ [] = error "Index out of range"
(!!) 0 (h:_) = h
(!!) n (_:t) = (!!) (n - 1) t

map f [] = []
map f (h : t) = f h : map f t

filter p [] = []
filter p (h:t) 
 | p h = h : filter p t
 | otherwise = filter p t

foldl op result [] = result
foldl op result (h : t) = foldl op (op result h) t 

foldr op nv [] = nv
foldr op nv (h : t) = op h (foldr op nv t)

take :: Int -> [a] -> [a]
take n [] = []
take 0 _ = []
take n (h : t) = h : (take (n - 1) t)

drop :: Int -> [a] -> [a]
drop _ [] = []
drop 0 list = list
drop n (h : t) = drop (n - 1) t

sorted [] = True
sorted [_] = True
sorted (a : b : t) = a <= b && sorted (b : t)

all p [] = True
all p (h : t) = p h && all p t

any p list = not (all (\ x -> not (p x)) list)
