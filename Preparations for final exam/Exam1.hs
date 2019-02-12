module Exam1 where

-- task 2
data Tree = Empty | Node { root :: Int
                         , left :: Tree
		         , right :: Tree 
		         }

makeLeaf :: Int -> Tree
makeLeaf value = Node value Empty Empty

tree = Node 5 
              (Node 3 Empty (makeLeaf 2))
	      (Node 4 (makeLeaf 6) Empty)

type Path = [Int]

sameAsCode :: Tree -> Int
sameAsCode tree = search tree [1]

search :: Tree -> Path -> Int
search Empty _ = 0
search (Node root left right) path 
 | root == fromBinary path = root
 | foundInLeft > 0 = foundInLeft
 | otherwise  = foundInRight
 where foundInLeft  = search left (0:path)
       foundInRight = search right (1:path) 

countSameAsCode :: Tree -> Int
countSameAsCode t = countCodes t [1]

countCodes :: Tree -> Path -> Int
countCodes Empty _ = 0
countCodes (Node root left right) path = sum [forRoot, inLeft, inRight]
 where forRoot = if root == fromBinary path then 1 else 0 
       inLeft  = countCodes left  (0:path)
       inRight = countCodes right (1:path)

fromBinary :: Path -> Int
fromBinary path = sum $ zipWith (\bit k -> bit * (2^k)) path powers
  where powers = [0 .. (length path) - 1] 
 
--task 3
descartes :: [[t]] -> [[t]]
descartes [] = [[]]
descartes (h:t) = [ x:y | x <- h, y <- descartes t ]

allEqual :: Eq f => [[t]] -> [(t -> f)] -> [[t]]
allEqual lists funs = filter p (descartes lists)
   where p xs = same $ zipWith ($) funs xs

same :: Eq t => [t] -> Bool
same []     = True
same (x:xs) = and $ map (==x) xs

-- task 4

data Ingredient = Ingredient { nameOf :: String, quantity :: Int }
  deriving (Eq, Ord, Show, Read)

data Medicine = Medicine { name :: String, ingredients :: [Ingredient] }
 deriving (Show, Read)

medicineA = Medicine "A" [(Ingredient "p" 6),(Ingredient "q" 9)] 
medicineB = Medicine "B" [(Ingredient "p" 2), (Ingredient "q" 3)] 
medicineC = Medicine "C" [(Ingredient "p" 3)] 
medicineD = Medicine "D" [(Ingredient "p" 6), (Ingredient "q" 9)]

isSubstitute :: Medicine -> Medicine -> Bool
isSubstitute (Medicine _ ingsLhs) (Medicine _ ingsRhs) = sameLengths && sameNames && sameProportions
  where sameNames = and $ zipWith (\x y -> nameOf x == nameOf y) ingsLhs ingsRhs
        sameProportions = same $ proportions ingsLhs ingsRhs
	sameLengths = length ingsLhs == length ingsRhs

proportions :: [Ingredient] -> [Ingredient] -> [Double]
proportions = zipWith (\(Ingredient _ x) (Ingredient _ y) -> (fromIntegral x) / (fromIntegral y)) 

bestSubstitute :: Medicine -> [Medicine] -> Medicine
bestSubstitute x@(Medicine _ ings) meds = best
 where (best, _) = maximumBy (\(_, (x:_)) (_, (y:_)) -> distance x <= distance y) pairs
       distance z = abs (1 - z)
       pairs = zip substitutes $ props
       props = map (\(Medicine _ ingsM) -> proportions ings ingsM) substitutes 
       substitutes = filter (isSubstitute x) meds

maximumBy :: (t -> t -> Bool) -> [t] -> t
maximumBy _ [] = error "Empty list passed to maximumBy"
maximumBy p (h:t) = foldl compare h t
 where compare max x = if p x max then x else max

groupSubstitutes :: [Medicine] -> [[Medicine]]
groupSubstitutes [] = []
groupSubstitutes (h:t) = (h:subs) : groupSubstitutes nonsubs
 where (subs, nonsubs) = splitBy (isSubstitute h) t

splitBy :: (t -> Bool) -> [t] -> ([t], [t])
splitBy p list = (first, second)
 where first  = filter p list
       second = filter (not . p) list
