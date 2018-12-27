module Trees where

data Tree t = Tree { root :: t, subtrees :: TreeList t }
  deriving (Eq, Ord, Show, Read)

data TreeList t = Empty | Subtrees { head :: Tree t, tail :: TreeList t }
  deriving (Eq, Ord, Show, Read)

leaf :: t -> Tree t
leaf x = Tree x Empty

t1 = Tree 1 $ Subtrees (leaf 2)
           $ Subtrees (Tree 3 $ Subtrees (leaf 4) $ Empty)
           $ Subtrees (leaf 5) $ Empty

t2 = Tree 1 $ Subtrees (leaf 2)
            $ Subtrees (Tree 3
                             $ Subtrees (Tree 2
                                              $ Subtrees (leaf 3)
                                              $ Subtrees (leaf 1)
                                              $ Subtrees (leaf 4)
                                              $ Empty)
                             $ Empty)
            $ Subtrees (leaf 4) $ Empty

level :: Int -> Tree t -> [t]
level 0 (Tree r _) = [r]
level n (Tree _ subtrees) = levelTrees (n - 1) subtrees

levelTrees :: Int -> TreeList t -> [t]
levelTrees _ Empty = []
levelTrees n (Subtrees h t) = level n h ++ levelTrees n t

familyOfRoot :: Tree t -> [t]
familyOfRoot (Tree root subtrees) = root:children
 where children = levelTrees 0 subtrees

haveSimilarFamilies :: (Eq t) => Tree t -> Tree t -> Bool
haveSimilarFamilies lhs rhs = equalAsSets (familyOfRoot lhs) (familyOfRoot rhs)

equalAsSets :: (Eq t) => [t] -> [t] -> Bool
equalAsSets lhs rhs = (isSubsetOf lhs rhs) && (isSubsetOf rhs lhs)

isSubsetOf :: (Eq t) => [t] -> [t] -> Bool
isSubsetOf lhs rhs = all ((flip elem) rhs) lhs

anySubtree :: (Tree t -> Bool) -> Tree t -> Bool
anySubtree p t = p t || anyInTreeList p (subtrees t)

anyInTreeList :: (Tree t -> Bool) -> TreeList t -> Bool
anyInTreeList _ Empty = False
anyInTreeList p (Subtrees h t) = anySubtree p h || anyInTreeList p t

subtreesLevel :: Int -> Tree t -> TreeList t
subtreesLevel 0 t = Subtrees t Empty
subtreesLevel n (Tree r subtrees) = subtreesLevelTL (n - 1) subtrees

subtreesLevelTL :: Int -> TreeList t -> TreeList t
subtreesLevelTL _ Empty = Empty
subtreesLevelTL n (Subtrees h t) = (subtreesLevel n h) +++ (subtreesLevelTL n t)

(+++) :: TreeList t -> TreeList t -> TreeList t
Empty +++ rhs = rhs
(Subtrees h t) +++ rhs = Subtrees h (t +++ rhs)
