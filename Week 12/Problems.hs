module Problems where

data Tree t = Empty | Node { value :: t, left :: Tree t, right :: Tree t }
  deriving (Eq, Ord, Show, Read)

t :: Tree Integer
t = Node 5 (Node 2 (Node 10 Empty Empty)
                   (Node 4 (Node 3 Empty Empty)
                           Empty))
           (Node 3 (Node 1 Empty Empty)
                   Empty)

level :: Integer -> Tree t -> [t]
level _ Empty = []
level 0 (Node x _ _) = [x]
level n (Node _ l r) = level (n - 1) l ++ level (n - 1) r

leaf :: t -> Tree t
leaf x = Node x Empty Empty

depth :: Tree t -> Integer
depth Empty = 0
depth (Node _ l r) = 1 + max (depth l) (depth r)

maxSumPath :: Tree Integer -> Integer
maxSumPath Empty = 0
maxSumPath (Node x Empty Empty) = x
maxSumPath (Node x l r) = x + max (maxSumPath l) (maxSumPath r)

prune :: Tree t -> Tree t
prune Empty = Empty
prune (Node _ Empty Empty) = Empty
prune (Node x l r) = Node x (prune l) (prune r)

bloomWith :: (t -> t) -> Tree t -> Tree t
bloomWith _ Empty = Empty
bloomWith f (Node x Empty Empty) = Node x newLeaf newLeaf
 where newLeaf = leaf $ f x
bloomWith f (Node x l r) = Node x (bloomWith f l) (bloomWith f r)

bloom = bloomWith id

rotateLeft :: Tree t -> Tree t
rotateLeft (Node p a (Node q b c)) = Node q (Node p a b) c

rotateRight :: Tree t -> Tree t
rotateRight (Node q (Node p a b) c) = Node p a (Node q b c) 

--treeMap :: (a -> b) -> Tree a -> Tree b
--treeMap _ Empty = Empty
--treeMap f (Node x l r) = Node (f x) (treeMap f l) (treeMap f r)

instance Functor Tree where
 fmap _ Empty = Empty
 fmap f (Node x l r) = Node (f x) (fmap f l) (fmap f r)

data BST t = BEmpty | BNode t (BST t) (BST t)
  deriving (Eq, Ord, Show, Read) 

bstInsert :: Ord t => t -> BST t -> BST t
bstInsert x BEmpty = BNode x BEmpty BEmpty
bstInsert x (BNode y left right) 
 | x < y = BNode y (bstInsert x left) right
 | otherwise = BNode y left (bstInsert x right)

bstSearch :: Ord t => t -> BST t -> Bool
bstSearch _ BEmpty = False
bstSearch x (BNode y left right)
 | x == y = True
 | otherwise = bstSearch x (if x < y then left else right)

bstToList :: BST t -> [t]
bstToList BEmpty = []
bstToList (BNode x l r) = (bstToList l) ++ x:bstToList r

bstSize :: BST t -> Int
bstSize BEmpty = 0
bstSize (BNode x l r) = 1 + (bstSize l) + (bstSize r)

listToBST :: Ord t => [t] -> BST t
listToBST = foldr bstInsert BEmpty

bstSort :: Ord t => [t] -> [t] 
bstSort = bstToList . listToBST
