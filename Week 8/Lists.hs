module Lists where
 
import Prelude hiding (head, tail, null, length, enumFromTo, enumFromThenTo, (++), reverse)

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
