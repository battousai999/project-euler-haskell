-- The number, 197, is called a circular prime because all rotations of the digits: 197, 971, and 719, are 
-- themselves prime.

-- There are thirteen such primes below 100: 2, 3, 5, 7, 11, 13, 17, 31, 37, 71, 73, 79, and 97.

-- How many circular primes are there below one million?

import Data.List (splitAt)

rotations :: Integer -> [Integer]
rotations num =
    let str = show num
        partitions = map (\x -> splitAt x str) [0..((length str) - 1)]
    in  map (\x -> read ((snd x) ++ (fst x))) partitions

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

isCircularPrime :: Integer -> Bool
isCircularPrime num = all isPrime $ rotations num

main = do
    let cPrimes n = filter isCircularPrime $ takeWhile (<n) primes
    putStrLn $ show $ length $ cPrimes 1000000
