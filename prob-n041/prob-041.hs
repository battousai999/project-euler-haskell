-- We shall say that an n-digit number is pandigital if it makes use of all the digits 1 to n 
-- exactly once. For example, 2143 is a 4-digit pandigital and is also prime.

-- What is the largest n-digit pandigital prime that exists?

import Data.List (nub, sort)
import Data.Numbers.Primes (primes)

isPandigital :: Integer -> Bool
isPandigital num =
    let vals = show num
        dedup = nub vals
    in  (length vals) == (length dedup) && (sort vals) == (take (length vals) "123456789")

main = do
    let panPrimes = filter isPandigital $ takeWhile (<1000000000) primes
    putStrLn $ show $ maximum panPrimes
