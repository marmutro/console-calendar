import Calendar

import Data.List
import Data.List.Split
import System.Environment

main = do 
    args <- getArgs
    thisYear <- getCurrentYear
    let cols = if length args >= 1 then read $ args !! 0 else 3
    let year = if length args >= 2 then read $ args !! 1 else thisYear
    let months = map showMonth (monthByWeek . byMonth $ datesInYear year)
    let cl = chunksOf cols months
    let tl = map transpose cl
    let lines = concat $ map joinStrings tl
    mapM_ putStrLn lines
