import Data.List
import Data.Time.Calendar
import Data.Time.Calendar.OrdinalDate
import Data.Time.Format
import System.Locale
import Text.Printf

datesInYear year = [beginOfYear year..endOfYear year] where
    beginOfYear year = fromGregorian year 1 1
    endOfYear year = addDays (-1) $ beginOfYear (year+1)
    
monthOfDate date = month
    where (_, month, _) = toGregorian date
    
dayOfDate date = day
    where (_, _, day) = toGregorian date
    
weekNumber date = week
    where (week, _) = mondayStartWeek date

monthName date = formatTime defaultTimeLocale "%B" date

listToString :: [a] -> (a -> String) -> String
listToString ls formatter = foldr (\v acc -> formatter v ++ acc) "" ls

main = do 
    let byMonth = groupBy (\a b -> (monthOfDate a) == (monthOfDate b))
    let byWeek = groupBy (\a b -> (weekNumber a) == (weekNumber b))
    let monthByWeek = map byWeek
    let printWeek ls = do
        -- putStr $ show (weekNumber $ head ls) ++ ":"
        putStr $ replicate (3*(7 - length ls)) ' ' 
        putStrLn $ listToString (map dayOfDate ls) (\v -> printf "%3d" v)
    let printMonth ls = do
        putStrLn $ monthName $ head $ head ls
        mapM_ printWeek ls
    mapM_ printMonth (monthByWeek . byMonth $ datesInYear 2015)
