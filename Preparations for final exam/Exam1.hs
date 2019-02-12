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
allEqual = allWithProperty same

allIncreasing :: (Num f, Ord f) => [[t]] -> [(t -> f)] -> [[t]]
allIncreasing = allWithProperty isArithmeticProg

allWithProperty :: ([f] -> Bool) -> [[t]] -> [(t -> f)] -> [[t]]
allWithProperty property lists funs = filter p (descartes lists)
  where p xs = property $ zipWith ($) funs xs

same :: Eq t => [t] -> Bool
same []     = True
same (x:xs) = and $ map (==x) xs

isArithmeticProg :: (Num t, Ord t) => [t] -> Bool
isArithmeticProg list = same $ zipWith (flip (-)) list (tail list)

-- task 4

data Ingredient = Ingredient { nameOf :: String, quantity :: Int }
  deriving (Show, Read)

instance Eq Ingredient where
 (Ingredient name1 _) == (Ingredient name2 _) = name1 == name2

instance Ord Ingredient where
 compare (Ingredient name1 _) (Ingredient name2 _) = compare name1 name2

data Medicine = Medicine { name :: String, ingredients :: [Ingredient] }
 deriving (Show, Read)

medicineA = Medicine "A" [(Ingredient "p" 6),(Ingredient "q" 9)] 
medicineB = Medicine "B" [(Ingredient "p" 2), (Ingredient "q" 3)] 
medicineC = Medicine "C" [(Ingredient "p" 3)] 
medicineD = Medicine "D" [(Ingredient "p" 6), (Ingredient "q" 9)]

isSubstitute :: Medicine -> Medicine -> Bool
isSubstitute lhs rhs = sameNames && sameProportions
  where sameNames = and $ zipWith (\x y -> nameOf x == nameOf y) ingsLhs ingsRhs
        sameProportions = same $ proportions ingsLhs ingsRhs
	[(Medicine _ ingsLhs), (Medicine _ ingsRhs)] = map sortIngredients [lhs, rhs]

sortIngredients :: Medicine -> Medicine
sortIngredients (Medicine name ingredients) = Medicine name $ sortByName ingredients
 where sortByName = sortBy (\lhs rhs -> nameOf lhs <= nameOf rhs)

sortBy :: Ord t => (t -> t -> Bool) -> [t] -> [t]
sortBy _ [] = []
sortBy compare (h:t) = smaller ++ h:greater
 where smaller = filter (compare h) t
       greater = filter (not . compare h) t

sort = sortBy (<=)

proportions :: [Ingredient] -> [Ingredient] -> [Double]
proportions = zipWith (\(Ingredient _ x) (Ingredient _ y) -> (fromIntegral x) / (fromIntegral y)) 

bestSubstitute :: Medicine -> [Medicine] -> Medicine
bestSubstitute x meds = best
 where (best, _) = maximumBy closestProportion pairs 
       closestProportion = (\(_, prop1) (_, prop2) -> distanceToOne prop1 <= distanceToOne prop2) 
       pairs = zip substitutes $ map head props
       props = map (proportions ingsX . ingredients) substitutes 
       substitutes = filter (isSubstitute x) $ withSortedIngredients
       ((Medicine _ ingsX):withSortedIngredients) = map sortIngredients (x:meds) 
      
distanceToOne :: Floating t => t -> t
distanceToOne = abs . (1-)

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

isStronger :: Medicine -> Medicine -> Bool
isStronger lhs@(Medicine _ ingsLhs) rhs@(Medicine _ ingsRhs) = isRhsSubset && lhsHasAtLeastSameQuantities && atLeastOneIsGreater   
 where isRhsSubset = isSubset ingsRhs ingsLhs 
       matchingIngs = matchingIngsInLhs lhs rhs
       quantitiesLhs = quantities matchingIngs
       quantitiesRhs = quantities $ sort ingsRhs
       lhsHasAtLeastSameQuantities = quantitiesLhs >= quantitiesRhs
       atLeastOneIsGreater = or $ zipWith (>) quantitiesLhs quantitiesRhs 
  
matchingIngsInLhs lhs rhs = sort [ x | x <- (ingredients lhs), elem x (ingredients rhs) ]

quantities :: [Ingredient] -> [Int]
quantities = map quantity

isSubset :: Eq t => [t] -> [t] -> Bool
isSubset lhs rhs = and $ map ((flip elem) rhs) lhs  

leastStronger :: Medicine -> [Medicine] -> String
leastStronger x meds 
 | null stronger = ""
 | otherwise     = let ((Medicine name _),_) = maximumBy (\(med1, sum1) (med2, sum2) -> sum1 <= sum2) pairs in name
 where stronger = filter (\med -> isStronger med x) meds
       sortedStronger = map sortIngredients stronger
       sumOfdiffs = map (sumOfdifferences x) sortedStronger
       pairs = zip sortedStronger sumOfdiffs
       
sumOfdifferences lhs rhs = sum $ zipWith (-) quantitiesLhs quantitiesRhs
  where matchingIngs = matchingIngsInLhs lhs rhs
        quantitiesLhs = quantities matchingIngs
	quantitiesRhs = quantities (ingredients rhs)

strongRelation :: [Medicine] -> [(Medicine, [String])]
strongRelation list = [ (med, names med list) | med <- list ]
 where names med list = map name stronger
        where stronger = filter (\m -> isStronger m med) list 
 
