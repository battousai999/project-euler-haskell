-- We shall say that an n-digit number is pandigital if it makes use of all the digits 1 to n exactly once; for example, the 
-- 5-digit number, 15234, is 1 through 5 pandigital.

-- The product 7254 is unusual, as the identity, 39 × 186 = 7254, containing multiplicand, multiplier, and product is 1 through 
-- 9 pandigital.

-- Find the sum of all products whose multiplicand/multiplier/product identity can be written as a 1 through 9 pandigital.

-- HINT: Some products can be obtained in more than one way so be sure to only include it once in your sum.

import Data.List (nub, sort)

isTriplePandigital :: (Integer, Integer, Integer) -> Bool
isTriplePandigital (val1, val2, val3) =
    let vals = concat [(show val1), (show val2), (show val3)]
        dedup = nub vals
    in  (length vals) == (length dedup) && (sort vals) == "123456789" 

-- The value (i.e., product) can only be part of a pandigital triple if it is, itself, pandigital
isPossiblePandigitalProduct :: Integer -> Bool
isPossiblePandigitalProduct num =
    let vals = show num
        dedup = nub vals
    in  (length vals) == (length dedup)

isProductPandigital :: Integer -> Bool
isProductPandigital num =
    let limit = ceiling $ sqrt $ fromIntegral num
        candidates = filter (\x -> num `mod` x == 0) [1..limit]
        results = filter (\x -> isTriplePandigital (x, num `div` x, num)) candidates
    in  any (\x -> isTriplePandigital (x, num `div` x, num)) candidates

main = do
    let isCandidate x = isPossiblePandigitalProduct x && isProductPandigital x
        -- The product cannot have more digits than 4 (i.e., 9 div 2), otherwise there would not
        -- be enough digits for the multiplicand and multiplier
        value = sum $ filter isCandidate [1..(10 ^ (9 `div` 2) - 1)]
    putStrLn $ show $ value
