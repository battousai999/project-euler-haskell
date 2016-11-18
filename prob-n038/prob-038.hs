-- Take the number 192 and multiply it by each of 1, 2, and 3:

-- 192 × 1 = 192
-- 192 × 2 = 384
-- 192 × 3 = 576

-- By concatenating each product we get the 1 to 9 pandigital, 192384576. We will call 192384576 
-- the concatenated product of 192 and (1,2,3)

-- The same can be achieved by starting with 9 and multiplying by 1, 2, 3, 4, and 5, giving the 
-- pandigital, 918273645, which is the concatenated product of 9 and (1,2,3,4,5).

-- What is the largest 1 to 9 pandigital 9-digit number that can be formed as the concatenated 
-- product of an integer with (1,2, ... , n) where n > 1?

import Data.List (nub, sort)

concatenatedProduct :: Integer -> Integer -> Integer
concatenatedProduct num tupleSize =
    let terms = map (*num) [1..tupleSize]
    in  read $ foldr (++) "" $ map show terms

isPandigital :: Integer -> Bool
isPandigital num =
    let vals = show num
        dedup = nub vals
    in  (length vals) == (length dedup) && (sort vals) == "123456789"

main = do
    let candidates = [concatenatedProduct a b |
            a <- [1..9999],
            b <- [2..(9 `div` (fromIntegral . length $ show a))]]
        value = maximum $ filter isPandigital candidates
    putStrLn $ show $ value
