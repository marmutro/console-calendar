module Calendar where

import Data.List
import Data.Time.Calendar
import Data.Time.Calendar.OrdinalDate
import Data.Time.Clock
import Data.Time.Format
-- import System.Locale
import Text.Printf

type Week = [Day]
type Month = [Week]

-- Converts a list into a string by using a formatter function
listToString :: [a] -> (a -> String) -> String
listToString ls formatter = foldr (\v acc -> formatter v ++ acc) "" ls

-- Join a list of strings with a space separator
joinStrings :: [[String]] -> [String]
joinStrings ls = map (\a -> intercalate " " a) ls

-- gives all days of a year
datesInYear :: Integer -> [Day]
datesInYear year = [beginOfYear year..endOfYear year] where
    beginOfYear year = fromGregorian year 1 1
    endOfYear year = addDays (-1) $ beginOfYear (year+1)
    
monthOfDate :: Day -> Int
monthOfDate date = month where
    (_, month, _) = toGregorian date
    
dayOfDate :: Day -> Int
dayOfDate date = day where
    (_, _, day) = toGregorian date

-- returns the calender week number (1..52)
weekNumber :: Day -> Int
weekNumber date = week where
    (week, _) = mondayStartWeek date

-- returns the week day number (monday==1 , sunday==7)
weekDay :: Day -> Int
weekDay date = day where
    (_, day) = mondayStartWeek date
    

monthName :: Day -> String
monthName date = printf "%-21s" $ formatTime defaultTimeLocale "%B" date

byMonth = groupBy (\a b -> (monthOfDate a) == (monthOfDate b))
byWeek = groupBy (\a b -> (weekNumber a) == (weekNumber b))
monthByWeek = map byWeek

showWeek :: Week -> String
showWeek ls = frontPadding ++ days ++ backPadding where
    frontPadding = replicate (3 * ((weekDay $ head ls) - 1)) ' '
    days = listToString (map dayOfDate ls) (\d -> printf "%3d" d)
    backPadding = replicate (3 * (7 - (weekDay $ last ls))) ' '
    
-- Every months is shown by exactly 7 lines: name, weeks + padding
showMonth :: Month -> [String]
showMonth ls = [showMonthName] ++ weeks ++ padding where
    showMonthName = (monthName $ head $ head ls)
    weeks = map showWeek ls
    padding = replicate (6 - (length ls)) emptyLine
    emptyLine = replicate 21 ' '
    
getCurrentYear = do
    utc <- getCurrentTime
    let now = utctDay utc
    let (year, _, _) = toGregorian now
    return year

