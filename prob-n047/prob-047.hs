-- The first two consecutive numbers to have two distinct prime factors are:

-- 14 = 2 × 7
-- 15 = 3 × 5

-- The first three consecutive numbers to have three distinct prime factors are:

-- 644 = 2^2 × 7 × 23
-- 645 = 3 × 5 × 43
-- 646 = 2 × 17 × 19.

-- Find the first four consecutive integers to have four distinct prime factors 
-- each. What is the first of these numbers?

factors :: Integer -> [Integer]
factors num =
    let limit = (ceiling . sqrt . fromIntegral) num 
        partialFactors = filter (\x -> num `mod` x == 0 && x /= num) [1..limit]
        factorFilter x 
            | x == limit && num `div` x == limit - 1 = []
            | num `div` x == x || x == 1             = [x]
            | otherwise                              = [x, num `div` x]
    in  concatMap factorFilter partialFactors

isPrime :: Integer -> Bool
isPrime num 
    | num <= 1 = False
    | otherwise =
        let limit = floor . sqrt . fromInteger $ num
        in  not $ any (\x -> num `mod` x == 0) [2..limit]

primeFactors :: Integer -> [Integer]
primeFactors num = filter isPrime $ factors num

calcValue :: Int -> Integer
calcValue num =
    let helper xs 
            | length cs == num = head cs
            | otherwise = helper (tail xs) 
            where cs = filter (\x -> length (primeFactors x) == num) $ take num xs
    in  helper [1..]

main = print $ calcValue 4
