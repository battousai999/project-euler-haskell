-- 145 is a curious number, as 1! + 4! + 5! = 1 + 24 + 120 = 145.

-- Find the sum of all numbers which are equal to the sum of the factorial of their digits.

-- Note: as 1! = 1 and 2! = 2 are not sums they are not included.

import Data.Char (digitToInt)

factorial :: Integer -> Integer
factorial n = product [1..n]

digits :: Integer -> [Integer]
digits num = map (fromIntegral . digitToInt) $ show num

curiousNumbers :: [Integer]
curiousNumbers = 
    let isCurious x = x == (sum $ map factorial $ digits x)
        -- After 7 digits, the sum of the factorial of the digits does not grow fast  
        -- enought to equal the number from which the digits where obtained
        limit = 7 * (factorial 9)
    in  filter isCurious [3..limit]

main = do
    putStrLn $ show $ sum curiousNumbers