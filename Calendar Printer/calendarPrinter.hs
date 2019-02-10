module CalendarPrinter where

import Data.Time.Calendar
import Data.Time.Calendar.WeekDate

data Date = Date { year :: Int
                 , month :: Int
		 , day :: Int }
       deriving (Eq, Ord)

dateFromString :: String -> Date
dateFromString str = Date year month day
    where year  = read $ take 4 str 
          month = read $ drop 5 $ take 7 str
	  day   = read $ drop 8 str

instance Show Date where
  show (Date y m d) = show d ++ "/" ++ show m ++ "/" ++ show y  

main :: IO ()
main = do
 year <- getYear
 let calendar = formatYear $ groupDatesOf year
 let withMonthNames = addMonthNames calendar
 let chunked = chunk 2  withMonthNames
 let zipped = map combine chunked
 mapM_ (mapM_ putStrLn) zipped
 putStrLn "" 

chunk :: Int -> [t] -> [[t]]
chunk _ []   = []
chunk n list = first : chunkedRest
 where (first, rest) = splitAt n list
       chunkedRest   = chunk n rest

rowWidth :: Int
rowWidth = 27

combine :: [[String]] -> [String]
combine [lhs, rhs] = zipWith (\x y -> x ++ space x y ++ y) lhs rhs
 where space x y = replicate n ' '
         where n = 3*rowWidth - (length x + length y)

getYear :: IO Integer
getYear = do
  putStr "Enter the year: "
  year <- getLine
  return $ read year

addMonthNames :: [[String]] -> [[String]]
addMonthNames = zipWith (:) names
 where names = zipWith (\month len -> prefix len ++ month) months lens
       months = ["January", "February", "March", "April",
                 "May", "June", "July", "August", 
		 "September", "October", "November", "December"]
       lens    = map length months
       prefix n = replicate (rowWidth - n) ' ' 
      
formatYear :: [[[Date]]] -> [[String]]
formatYear year = alignMonths months
 where months = map formatMonth year

alignMonths :: [[String]] -> [[String]]
alignMonths months = map (paddMonth maxLen) months
  where maxLen = maximum $ map length months 
	 		   
paddMonth :: Int -> [String] -> [String]
paddMonth maxLen month = month ++ padding 
 where padding = replicate n emptyRow
       emptyRow = replicate rowWidth ' '
       n = maxLen - length month

formatMonth :: [[Date]] -> [String]
formatMonth = map formatWeek 

formatWeek :: [Date] -> String
formatWeek = alignWeek . concat . asStrings  
  where asStrings = map toCalendarDate
        toCalendarDate (Date _ _ day) = prefix ++ show day ++ suffix
	 where prefix = if day < 10 then "  " else  " "
               suffix = " "

alignWeek :: String -> String
alignWeek week@(_:date:t)
 | len == rowWidth              = week 
 | len < rowWidth && date == '1' = padding ++ week
 | otherwise                    = week ++ padding
 where len = length week
       padding = replicate (rowWidth - len) ' '

groupDatesOf :: Integer -> [[[Date]]]
groupDatesOf year = map (groupBy weekOf) groupedByMonth
 where  groupedByMonth = groupBy month dates
        dates          = datesOf year

weekOf :: Date -> Int
weekOf (Date y m d) = week
  where (_, week, _) = toWeekDate $ fromGregorian (toInteger y) m d

datesOf :: Integer -> [Date]
datesOf year = init dates
 where dates = map (dateFromString . show) [start .. end]
       start = fromGregorian year jan 1
       end   = fromGregorian (year + 1) jan 1
       jan   = 1

groupBy :: (Eq key) => (item -> key) -> [item] -> [[item]]
groupBy _ []     = []
groupBy f (x:xs) = (x:similar) : groupBy f different 
 where (similar, different) = splitBy (\y -> f y == k) xs
       k = f x

splitBy :: (item -> Bool) -> [item] -> ([item], [item])
splitBy p items  = (xs, ys)
 where xs = filter p items
       ys = filter (not . p) items 
