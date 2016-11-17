-- The number 3797 has an interesting property. Being prime itself, it is possible to 
-- continuously remove digits from left to right, and remain prime at each stage: 3797, 797, 
-- 97, and 7. Similarly we can work from right to left: 3797, 379, 37, and 3.

-- Find the sum of the only eleven primes that are both truncatable from left to right and 
-- right to left.

-- NOTE: 2, 3, 5, and 7 are not considered to be truncatable primes.

import Data.List (tails, inits)

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

isTruncatable_lr :: Integer -> Bool
isTruncatable_lr num =
    let vals = map read $ init $ tails $ show num
    in  all isPrime vals

isTruncatable_rl :: Integer -> Bool
isTruncatable_rl num =
    let vals = map read $ tail $ inits $ show num
    in  all isPrime vals

main = do
    let candidatePrimes = dropWhile (<=7) primes
        isTruncatable x = isTruncatable_lr x && isTruncatable_rl x
        value = sum $ take 11 $ filter isTruncatable candidatePrimes
    putStrLn $ show $ value
