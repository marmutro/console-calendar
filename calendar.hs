import Data.List
import Data.Time.Calendar
import Data.Time.Calendar.OrdinalDate

datesInYear year = [beginOfYear year..endOfYear year] where
    beginOfYear year = fromGregorian year 1 1
    endOfYear year = addDays (-1) $ beginOfYear (year+1)
    
monthOfDate date = month
    where (year, month, day) = toGregorian date
    
dayOfDate date = day
    where (year, month, day) = toGregorian date
    
weekNumber date = week
    where (week, dayOfWeek) = mondayStartWeek date

main = do 
    let byMonth =  groupBy (\a b -> (monthOfDate a) == (monthOfDate b))
    let byWeek = groupBy (\a b -> (weekNumber a) == (weekNumber b))
    let monthByWeek = map byWeek
    let printDayOfWeekDate ls = do
        print $ map dayOfDate ls
    let printDayOfMonthDate ls = do
        mapM_ printDayOfWeekDate ls
        print "---"
    mapM_ printDayOfMonthDate (monthByWeek . byMonth $ datesInYear 2015)

