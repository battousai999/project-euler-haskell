-- Euler discovered the remarkable quadratic formula:

-- n^2+n+41

-- It turns out that the formula will produce 40 primes for the consecutive integer values 0≤n≤39. However, when 
-- n=40, 40^2+40+41 = 40(40+1)+41 is divisible by 41, and certainly when n=41,41^2+41+41 is clearly divisible by 41.

-- The incredible formula n^2−79n+1601 was discovered, which produces 80 primes for the consecutive values 0≤n≤79. The 
-- product of the coefficients, −79 and 1601, is −126479.

-- Considering quadratics of the form:

-- n^2+an+b, where |a|<1000 and |b|≤1000

-- where |n| is the modulus/absolute value of n
-- e.g. |11|=11 and |−4|=4

-- Find the product of the coefficients, a and b, for the quadratic expression that produces the maximum number of primes 
-- for consecutive values of n, starting with n=0.

import Data.List (maximumBy)

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

numConsecutivePrimes :: Integer -> Integer -> Int
numConsecutivePrimes a b =
    let f n = (n * n) + (a * n) + b
    in  length $ takeWhile (isPrime . f) [0..]

calcValue :: Integer -> Integer -> Integer
calcValue aLimit bLimit =
    let aLower = 1 - aLimit
        aUpper = aLimit - 1
        cs = [((a, b), numConsecutivePrimes a b) | 
            a <- [aLower, (aLower + 1) .. aUpper],
            b <- takeWhile (<= bLimit) primes] 
        (a, b) = fst $ maximumBy (\x y -> (snd x) `compare` (snd y)) cs
    in  a * b

main = do
    (putStrLn . show) $ calcValue 1000 1000

