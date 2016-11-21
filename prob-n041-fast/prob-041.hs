-- We shall say that an n-digit number is pandigital if it makes use of all the digits 1 to n 
-- exactly once. For example, 2143 is a 4-digit pandigital and is also prime.

-- What is the largest n-digit pandigital prime that exists?

import Data.List (permutations)

pandigitals :: [Integer]
pandigitals =
    let spoints = tail $ scanl (++) "" ["1", "2", "3", "4", "5", "6", "7", "8", "9"]
    in  map read $ concat $ map permutations spoints

isPrime :: Integer -> Bool
isPrime num 
    | num <= 1 = False
    | otherwise =
        let limit = floor . sqrt . fromInteger $ num
        in  not $ any (\x -> num `mod` x == 0) [2..limit]

main = do
    putStrLn $ show $ maximum $ filter isPrime pandigitals    
