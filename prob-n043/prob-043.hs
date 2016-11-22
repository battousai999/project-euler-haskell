-- The number, 1406357289, is a 0 to 9 pandigital number because it is made up of each of the 
-- digits 0 to 9 in some order, but it also has a rather interesting sub-string divisibility property.

-- Let d1 be the 1st digit, d2 be the 2nd digit, and so on. In this way, we note the following:

-- d_2 ++ d_3 ++ d_4=406 is divisible by 2
-- d_3 ++ d_4 ++ d_5=063 is divisible by 3
-- d_4 ++ d_5 ++ d_6=635 is divisible by 5
-- d_5 ++ d_6 ++ d_7=357 is divisible by 7
-- d_6 ++ d_7 ++ d_8=572 is divisible by 11
-- d_7 ++ d_8 ++ d_9=728 is divisible by 13
-- d_8 ++ d_9 ++ d_10=289 is divisible by 17

-- Find the sum of all 0 to 9 pandigital numbers with this property.

import Data.List (permutations)

pandigitals :: [Integer]
pandigitals = map read $ filter (\x -> x !! 0 /= '0') $ permutations "0123456789"

isSubStringDivisible :: Integer -> Bool
isSubStringDivisible num =
    let applicablePrimes = [2, 3, 5, 7, 11, 13, 17]
        sub x = read $ (take 3) . (drop (x-1)) $ show num
    in  all (== True) $ map (\x -> (sub (x+1)) `mod` (applicablePrimes !! (x-1)) == 0) [1..7]

main = do
    let value = sum $ filter isSubStringDivisible pandigitals
    putStrLn $ show value
