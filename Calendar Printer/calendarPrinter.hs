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
  show (Date year month day) = show day ++ "/" ++ show month ++ "/" ++ show year  

main :: IO ()
main = do
 year <- getYear
 let dates = groupDatesOf year
 let formated = formatYear dates
 let withMonthNames = addMonthNames formated
 putStrLn ""

chunk :: Int -> [t] -> [[t]]
chunk _ []   = []
chunk n list = first : chunkedRest
 where (first, rest) = splitAt n list
       chunkedRest   = chunk n rest

getYear :: IO Integer
getYear = do
  putStr "Enter the year: "
  year <- getLine
  return $ read year

addMonthNames :: [[String]] -> [[String]]
addMonthNames = zipWith (:) names
 where names = ["January", "February", "March", "April",
                "May", "June", "July", "August", 
		"September", "October", "November", "December"]

formatYear :: [[[Date]]] -> [[String]]
formatYear year = alignMonths months
 where months = map formatMonth year

alignMonths :: [[String]] -> [[String]]
alignMonths months = map (paddMonth maxLen) months
  where maxLen = maximum $ map length months 
	 		   
paddMonth :: Int -> [String] -> [String]
paddMonth maxLen month = month ++ padding 
 where padding = replicate n emptyRow
       emptyRow = replicate 35 ' '
       n = maxLen - length month

formatMonth :: [[Date]] -> [String]
formatMonth = map formatWeek 

formatWeek :: [Date] -> String
formatWeek = concat . map (\(Date _ _ day) -> " " ++ show day ++ " ")

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
