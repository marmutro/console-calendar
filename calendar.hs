import Data.List
import Data.List.Split
import Data.Time.Calendar
import Data.Time.Calendar.OrdinalDate
import Data.Time.Format
import System.Locale
import Text.Printf

type Week = [Day]
type Month = [Week]

listToString :: [a] -> (a -> String) -> String
listToString ls formatter = foldr (\v acc -> formatter v ++ acc) "" ls

joinStrings :: [[String]] -> [String]
joinStrings ls = map (\a -> intercalate " " a) ls


datesInYear year = [beginOfYear year..endOfYear year] where
    beginOfYear year = fromGregorian year 1 1
    endOfYear year = addDays (-1) $ beginOfYear (year+1)
    
monthOfDate date = month where
    (_, month, _) = toGregorian date
    
    
weekNumber date = week where
    (week, _) = mondayStartWeek date

weekDay date = day where
    (_, day) = mondayStartWeek date
    
dayOfDate date = day where
    (_, _, day) = toGregorian date


monthName date = printf "%-21s" $ formatTime defaultTimeLocale "%B" date

byMonth = groupBy (\a b -> (monthOfDate a) == (monthOfDate b))
byWeek = groupBy (\a b -> (weekNumber a) == (weekNumber b))
monthByWeek = map byWeek

showWeek :: Week -> String
showWeek ls = frontPadding ++ days ++ backPadding where
    frontPadding = replicate (3 * ((weekDay $ head ls) - 1)) ' '
    days = listToString (map dayOfDate ls) (\d -> printf "%3d" d)
    backPadding = replicate (3 * (7 - (weekDay $ last ls))) ' '
    
showMonth :: Month -> [String]
showMonth ls = [showMonthName] ++ weeks ++ padding where
    showMonthName = (monthName $ head $ head ls)
    weeks = map showWeek ls
    padding = replicate (6 - (length ls)) emptyLine
    emptyLine = replicate 21 ' '
    

main = do 
    let months = map showMonth (monthByWeek . byMonth $ datesInYear 2015)
    let cl = chunksOf 3 months
    let tl = map transpose cl
    let lines = concat $ map joinStrings tl
    mapM_ putStrLn lines
