module Eaxam where

-- task 2
-- Graph : [v:[neighbours] , ... ]

type Graph = [[Int]]
g :: Graph
g = [1:[2,3,4], 2:[1,3], 3:[2,4], 4:[1]]

vertices :: Graph -> [Int]
vertices = map head

children :: Int -> Graph -> [Int]
children u [] = []
children u ((v:vs):t) 
 | u == v = vs
 | otherwise = children u t

findNumber :: Int -> Graph -> Bool
findNumber n g = existsPath g n initialPaths
 where initialPaths = map (:[]) (vertices g)

size :: Int -> Int
size 0 = 0
size n = 1 + size (n `div` 10)

existsPath _ _ [] = False
existsPath g n paths 
 | all (>size n) $ map length paths = False
 | any (equalTo n) paths = True
 | otherwise = existsPath g n (extend paths g) 

extendAcyclic :: [[Int]] -> Graph -> [[Int]]
extendAcyclic paths g = filter noCycle $ extend paths g  
 where noCycle (h:t) = not $ elem h t

extend :: [[Int]] -> Graph -> [[Int]]
extend paths g = concat $ map (extendPath g) paths

extendPath :: Graph -> [Int] -> [[Int]]
extendPath g path@(u:_) = map (:path) (children u g) 

equalTo :: Int -> [Int] -> Bool
equalTo _ [] = False
equalTo n path = n == toDecimal path

toDecimal :: [Int] -> Int
toDecimal path = sum $ zipWith (\num pow -> num * (10^pow)) path [0 .. (length path) - 1] 

allNumbers :: Int -> Graph -> [Int]
allNumbers d g = [ n | n <- nums, findNumber n g ]
 where nums = filter ((==d). firstDigit) [1..]
       firstDigit m = m `div` 10^((size m) - 1)

-- task 3

data Movie = Movie { name :: String
                   , price :: Double
		   , time :: Int 
		   }
  deriving (Show)

instance Eq Movie where
 (Movie name1 _ _) == (Movie name2 _ _) = name1 == name2

movies :: [Movie]
movies = [(Movie "Batman" 17.99 126), (Movie "Manhattan" 7.99 96),
          (Movie "Alien" 14.99 116),  (Movie "Amadeus" 15.98 160)]

longestMovie :: Double -> [Movie] -> Maybe String
longestMovie budget movies = case result of
 Nothing -> Nothing 
 Just (Movie name _ _) -> Just name
 where result = maxByTime canAfford
       maxByTime = maximumBy (\(Movie _ _ t1) (Movie _ _ t2) -> t1 >= t2) 
       canAfford = filter ((<= budget) . price) movies

maximumBy ::(t -> t -> Bool) -> [t] -> Maybe t
maximumBy _ [] = Nothing
maximumBy p (h:t) = Just $ foldl compare h t
 where compare max x = if p x max then x else max

bestPlaylist :: Int -> Double -> [Movie] -> Maybe [Movie]
bestPlaylist time budget movies = closestToBudget acceptableSubsets
 where closestToBudget = maximumBy (\lhs rhs -> distToBudget lhs <= distToBudget rhs)     
       distToBudget subset = budget - totalPrice subset
       acceptableSubsets = filter acceptPriceAndTime $ allSubsets movies
       acceptPriceAndTime movies = totalPrice movies <= budget && meanTime movies <= (fromIntegral time)

totalPrice :: [Movie] -> Double
totalPrice = sum . map price

meanTime :: [Movie] -> Double
meanTime movies = (fromIntegral totalTime) / (fromIntegral $ length movies)
 where totalTime = sum $ map time movies

allSubsets :: Eq t => [t] -> [[t]]
allSubsets list = concat $ map ((flip allLists) list) [0 .. length list]

allLists :: Eq t => Int -> [t] -> [[t]]
allLists 0 _ = [[]]
allLists n list = [ x:xs | x <- list, xs <- allLists (n - 1) $ remove x list ]

remove :: Eq t => t -> [t] -> [t]
remove _ [] = []
remove x (h:t) 
 | h == x = t
 | otherwise = h : remove x t






