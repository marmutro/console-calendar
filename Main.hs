import Calendar

import System.Environment


main = do 
    args <- getArgs
    thisYear <- getCurrentYear
    let cols = if length args >= 1 then read $ args !! 0 else 3
    let year = if length args >= 2 then read $ args !! 1 else thisYear
    let lines = showCalendar year cols
    mapM_ putStrLn lines
