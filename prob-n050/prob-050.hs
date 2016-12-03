-- The prime 41, can be written as the sum of six consecutive primes:

-- 41 = 2 + 3 + 5 + 7 + 11 + 13

-- This is the longest sum of consecutive primes that adds to a prime below one-hundred.

-- The longest sum of consecutive primes below one-thousand that adds to a prime, contains 21 terms, and is equal to 953.

-- Which prime, below one-million, can be written as the sum of the most consecutive primes?

import Data.Maybe (fromMaybe)

isPrime :: Integer -> Bool
isPrime num 
    | num <= 1 = False
    | otherwise =
        let limit = floor . sqrt . fromInteger $ num
        in  not $ any (\x -> num `mod` x == 0) [2..limit]

primes :: [Integer]
primes = 
    let helper x = if isPrime x then x : helper (x + 2) else helper (x + 2)
    in  2 : 3 : helper 5

limitedTails :: Integer -> [Integer] -> [[Integer]]
limitedTails limit xs 
    | sum xs <= limit = []
    | otherwise = xs : limitedTails limit (tail xs)
    where s = sum xs

limitedInits :: Integer -> [Integer] -> [[Integer]]
limitedInits limit xs
    | sum xs <= limit = []
    | otherwise = xs : limitedInits limit (init xs)

consecutiveSubsets :: Integer -> [Integer] -> [[Integer]]
consecutiveSubsets _ [] = []
consecutiveSubsets limit xs 
    | sum xs <= limit = []
    | otherwise = xs : (limitedTails limit (tail xs) ++ limitedInits limit (init xs) ++ consecutiveSubsets limit ((tail . init) xs))

safeHead [] = Nothing
safeHead (x:_) = Just x

--calcValue :: Integer -> Integer
calcValue limit =
    let primesUnderLimit = takeWhile (<limit) primes
        l = last primesUnderLimit
        f xs = 
            let s = sum xs
            in  length xs > 1 && s < limit && isPrime s
    in  length $ filter f $ consecutiveSubsets l primesUnderLimit
