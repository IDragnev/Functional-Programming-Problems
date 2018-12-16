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
