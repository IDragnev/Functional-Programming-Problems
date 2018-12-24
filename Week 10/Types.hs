module Types where

type Hash k v = [(k, v)]
type UnaryFunction t = t -> t

keys :: Hash k v -> [k]
keys = map fst

values :: Hash k v -> [v]
values = map snd

class Measurable t where
 size :: t -> Int
 empty :: t -> Bool
 empty = (==0) . size
 
larger :: Measurable t => t -> t -> Bool
larger lhs rhs = size lhs > size rhs

instance Measurable Int where
 size 0 = 0
 size n = 1 + size (n `div` 10)

instance (Measurable a, Measurable b) => Measurable (a,b) where
 size (x, y) = size x + size y

instance Measurable a => Measurable [a] where
 size = sum . map size

type Name = String
type Score = Int
data Player = Player Name Score

getName (Player name _) = name
getScore (Player _ score) = score

data Person = Person { name :: String, age :: Int } 

data Weekday = Mon | Tue | Wed | Thu | Fri | Sat | Sun
               deriving (Eq, Ord, Show, Read, Enum)

data Nat = Zero | Next Nat
   deriving (Eq, Show, Read, Ord)

fromNat :: Nat -> Int
fromNat Zero = 0
fromNat (Next n) = 1 + (fromNat n)

toNat :: Int -> Nat
toNat 0 = Zero
toNat n = Next $ toNat $ n - 1 

plus :: Nat -> Nat -> Nat
plus Zero n = n
plus (Next m) n = Next (plus m n) 

multiply :: Nat -> Nat -> Nat
multiply _ Zero = Zero
multiply Zero _ = Zero
--multiply lhs (Next Zero) = lhs
--multiply (Next Zero) rhs = rhs
multiply (Next m) n = plus n (multiply m n)

data Binary = One | BitOne Binary | BitZero Binary
   deriving (Eq, Ord, Show, Read)

toBinary :: Int -> Binary
toBinary 1 = One
toBinary n
 | even n = BitZero m
 | otherwise = BitOne m
 where m = toBinary (n `div` 2)

fromBinary :: Binary -> Integer
fromBinary One = 1
fromBinary (BitOne n) = (2 * fromBinary n) + 1 
fromBinary (BitZero n) = 2 * fromBinary n 

nextBinary :: Binary -> Binary
nextBinary One = BitZero One
nextBinary (BitZero b) = BitOne b
nextBinary (BitOne b) = BitZero (nextBinary b)

data List t = Nil | Cons { head :: t, tail :: List t }
  deriving (Eq, Ord, Show, Read)

fromList :: List t -> [t]
fromList Nil = []
fromList (Cons h t) = h : fromList t

(+++) :: List t -> List t -> List t
Nil +++ rhs = rhs
(Cons h t) +++ rhs = Cons h (t +++ rhs)


len :: List t -> Integer
len Nil = 0
len (Cons h t) = 1 + len t


toList [] = Nil
toList (h:t) = Cons h (toList t)

data BinaryTree t = Empty | Node { value :: t, left :: BinaryTree t, right :: BinaryTree t }
 deriving (Eq, Ord, Show, Read)

makeLeaf x = Node x Empty Empty

depth :: BinaryTree t -> Int
depth Empty = 0
depth (Node v l r) = 1 + max (depth l) (depth r)

leaves :: BinaryTree t -> [t]
leaves Empty = []
leaves (Node v Empty Empty) = [v]
leaves (Node _ l r) = leaves l ++ leaves r

mapBinaryTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapBinaryTree _ Empty = Empty
mapBinaryTree f (Node v l r) = Node (f v) (mapBinaryTree f l) (mapBinaryTree f r)

foldrBT :: (a -> b -> b) -> b -> BinaryTree a -> b
foldrBT _ nv Empty = nv
foldrBT op nv (Node v l r) =   foldrBT op (v `op` (foldrBT op nv r)) l

foldlBT :: (b -> a -> b) -> b -> BinaryTree a -> b
foldlBT _ nv Empty = nv
foldlBT op nv (Node v l r) = foldlBT op ((foldlBT op nv l) `op` v) r

data Tree t = Tree { root :: t, subtrees :: TreeList t }
  deriving (Eq, Ord, Show, Read)

data TreeList t = None | Subtrees { first :: Tree t, rest :: TreeList t }
  deriving (Eq, Ord, Show, Read)

makeLeafTree v = Tree v None

treeDepth (Tree _ ts) = 1 + maxDepthTrees ts

maxDepthTrees :: TreeList a -> Integer
maxDepthTrees None = 0
maxDepthTrees (Subtrees first rest) = max (treeDepth first) (maxDepthTrees rest)

level 0 (Tree x _) = [x]
level n (Tree x ts) = levelTrees (n - 1) ts

levelTrees :: Integer -> TreeList t -> [t]
levelTrees _ None = []
levelTrees n (Subtrees first rest) = level n first ++ levelTrees n rest
