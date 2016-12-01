-- It was proposed by Christian Goldbach that every odd composite number can be written as the sum 
-- of a prime and twice a square.

-- 9 = 7 + 2×1^2
-- 15 = 7 + 2×2^2
-- 21 = 3 + 2×3^2
-- 25 = 7 + 2×3^2
-- 27 = 19 + 2×2^2
-- 33 = 31 + 2×1^2

-- It turns out that the conjecture was false.

-- What is the smallest odd composite that cannot be written as the sum of a prime and twice a square?

isPrime :: Integer -> Bool
isPrime num 
    | num <= 1 = False
    | otherwise =
        let limit = floor . sqrt . fromInteger $ num
        in  not $ any (\x -> num `mod` x == 0) [2..limit]

oddCompositeNumbers :: [Integer]
oddCompositeNumbers = 
    let helper x = if (not . isPrime) x then x : helper (x + 2) else helper (x + 2)
    in  helper 9

primes :: [Integer]
primes = 
    let helper x = if isPrime x then x : helper (x + 2) else helper (x + 2)
    in  2 : 3 : helper 5

squares :: [Integer]
squares = map (\x -> x * x) [1..]

isGoldbach :: Integer -> Bool
isGoldbach num =
    let candidates = [p + 2 * s |
                        p <- takeWhile (<num) primes,
                        s <- takeWhile (<num) squares]
    in  num `elem` candidates

main = do
    let answer = head $ dropWhile isGoldbach oddCompositeNumbers
    print answer
