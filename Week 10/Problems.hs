module Problems where

import Prelude hiding (foldr, scanl, scanr)

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _ nv [] = nv
foldr op nv (h:t) = h `op` (foldr op nv t) 

-- scanl op nv [x1, x2, ... ,xn]
-- [nv, nv `op` x1, (x1 `op` nv) `op` x2, ... , result' `op` xn] 
scanl :: (b -> a -> b) -> b -> [a] -> [b]
scanl _ nv [] = nv : []
scanl op nv (h:t) = nv : scanl op (nv `op` h) t

-- scanr op nv [x1, x2, ... , xn]
-- [x1 `op` (x2 `op` ...),  
--  x2 `op` (x3 `op` ...),
--  ...
--  xn-1 `op` (xn `op` nv),
--  xn `op` nv,
--  nv]
scanr :: (a -> b -> b) -> b -> [a] -> [b]
--scanr _ nv [] = nv : []
--scanr op nv (h:t) = (h `op` rh) : rest
-- where rest@(rh:_) = scanr op nv t
scanr op nv = foldr (\x rest@(rh:_) -> (x `op` rh) : rest) [nv]

enumFromStep from step = from : enumFromStep (from + step) step

sumProducts :: (Num c, Foldable t) => [t c] -> c
sumProducts = sum . map product

timesFound :: Eq t => t -> [t] -> Int
timesFound _ [] = 0
timesFound x (h:t) 
 | x == h = 1 + timesInTail
 | otherwise = timesInTail
 where timesInTail = timesFound x t

occurrences :: Eq t => [t] -> [t] -> [Int]
occurrences lhs rhs = map (\x -> timesFound x rhs) lhs

matchingLengths :: [[t]] -> Bool
matchingLengths [] = True
matchingLengths list = all (==h) lens
 where lens@(h:_) = map length list
   
setInsert :: Ord t => t -> [t] -> [t]    
setInsert x [] = [x]
setInsert x set@(h:t) 
 | x == h = set
 | x > h = h:setInsert x t
 | otherwise = x:set

setUnion :: Ord t => [t] -> [t] -> [t]
setUnion lhs rhs = foldr setInsert rhs lhs

setIntersect :: Eq t => [t] -> [t] -> [t]
setIntersect lhs rhs = filter ((flip elem) rhs) lhs

setDiff :: Eq t => [t] -> [t] -> [t]
setDiff lhs rhs = filter (\x -> not (elem x rhs)) lhs
