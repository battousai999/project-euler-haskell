-- The arithmetic sequence, 1487, 4817, 8147, in which each of the terms increases by 3330, is unusual in two ways: (i) each 
-- of the three terms are prime, and, (ii) each of the 4-digit numbers are permutations of one another.

-- There are no arithmetic sequences made up of three 1-, 2-, or 3-digit primes, exhibiting this property, but there is one 
-- other 4-digit increasing sequence.

-- What 12-digit number do you form by concatenating the three terms in this sequence?

import Data.List (permutations)

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

isPermutation :: Integer -> Integer -> Bool
isPermutation n1 n2 = show n1 `elem` permutations (show n2)

calcValue :: [Integer]
calcValue =
    let fourDigitPrimes = filter (\x -> x >= 1000 && x <= 9999) primes
        isSpecial x i = 
            let x2 = x + i
                x3 = x2 + i
            in  (isPrime x2 && isPermutation x x2) && (isPrime x3 && isPermutation x x3)
        result = head [(num, inc) |
                        num <- fourDigitPrimes,
                        inc <- [500..4500],
                        num /= 1487,
                        isSpecial num inc]
    in  [fst result, fst result + snd result, fst result + 2 * snd result]

main = putStrLn $ concatMap show calcValue