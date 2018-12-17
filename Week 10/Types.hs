module Types where

type List t = [t]
type Hash k v = [(k, v)]
type UnaryFunction t = t -> t

keys :: Hash k v -> [k]
keys = map fst

values :: Hash k v -> [v]
values = map snd

class Measurable t where
 size :: t -> Int
 empty :: t -> Bool
 empty x = size x == 0
 
larger :: Measurable t => t -> t -> Bool
larger lhs rhs = size lhs > size rhs

instance Measurable Int where
 size 0 = 0
 size n = 1 + size (n `div` 10)

instance (Measurable a, Measurable b) => Measurable (a,b) where
 size (x, y) = size x + size y

instance Measurable a => Measurable [a] where
 size = sum . map size
