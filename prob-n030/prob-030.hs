-- Surprisingly there are only three numbers that can be written as the sum of fourth powers of their digits:

-- 1634 = 1^4 + 6^4 + 3^4 + 4^4
-- 8208 = 8^4 + 2^4 + 0^4 + 8^4
-- 9474 = 9^4 + 4^4 + 7^4 + 4^4
-- As 1 = 1^4 is not a sum it is not included.

-- The sum of these numbers is 1634 + 8208 + 9474 = 19316.

-- Find the sum of all the numbers that can be written as the sum of fifth powers of their digits.

import Data.Char (digitToInt)

isDigitNthPowerSum :: Int -> Integer -> Bool
isDigitNthPowerSum power num =
    let digits = map digitToInt $ show num
        s = sum $ map (^power) digits
    in  s == (fromIntegral num)

digitNthPowerSums :: Int -> [Integer]
digitNthPowerSums power = 
    let limit = 10 ^ ((fromIntegral power) + 1) - 1
    in  filter (isDigitNthPowerSum power) [2..limit]

main = do
    let s = sum $ digitNthPowerSums 5
    putStrLn $ show s
