-- You are given the following information, but you may prefer to do some research for yourself.

-- 1 Jan 1900 was a Monday.
-- Thirty days has September,
-- April, June and November.
-- All the rest have thirty-one,
-- Saving February alone,
-- Which has twenty-eight, rain or shine.
-- And on leap years, twenty-nine.
-- A leap year occurs on any year evenly divisible by 4, but not on a century unless it is divisible by 400.

-- How many Sundays fell on the first of the month during the twentieth century (1 Jan 1901 to 31 Dec 2000)?

data DayOfWeek = Sun | Mon | Tue | Wed | Thu | Fri | Sat 
    deriving (Enum, Show, Eq)

data Date = Date { year :: Int, month :: Int, day :: Int } 
    deriving (Show, Eq)

isLeapYear :: Int -> Bool
isLeapYear y = (y `mod` 4 == 0) && ((y `mod` 400 == 0) || (y `mod` 100 /= 0))

daysInYear :: Int -> Int
daysInYear y = if isLeapYear y then 366 else 365

dayOfYear :: Date -> Int
dayOfYear date =
    let febDays = if isLeapYear (year date) then 29 else 28
        monthDays = [31, febDays, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    in  ((day date)+) . sum $ take ((month date) - 1) monthDays

instance Enum Date where
    fromEnum date = 
        let yearDays = sum $ map daysInYear [1900..((year date) - 1)]
            days = dayOfYear date
        in  yearDays + days - 1
    toEnum num =
        let numOrd = num + 1
            yearDaysAccum = zip [1900..] $ scanl (+) 1 $ map daysInYear [1900..]
            yearDaysList = takeWhile ((numOrd >=) . snd) yearDaysAccum 
            (eYear, yearDays) = last yearDaysList
            febDays = if isLeapYear eYear then 29 else 28
            monthDaysIn = [31, febDays, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
            monthDaysAccum = zip [1..12] $ scanl (+) 1 monthDaysIn
            eDayOfYear = numOrd - yearDays + 1
            monthDaysList = takeWhile ((eDayOfYear >=) . snd) monthDaysAccum
            (eMonth, monthDays) = last monthDaysList
            eDay = eDayOfYear - monthDays + 1
        in  Date eYear eMonth eDay

calcDayOfWeek :: Date -> DayOfWeek
calcDayOfWeek date = toEnum (((fromEnum date) + 1) `mod` 7)

calcResults :: Int
calcResults =
    let firstOfMonths = [Date y m 1 | y <- [1901..2000], m <- [1..12]]
    in  length $ filter (\x -> (calcDayOfWeek x) == Sun) firstOfMonths

main = do
    putStrLn $ show $ calcResults
