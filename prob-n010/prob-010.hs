-- The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.
-- Find the sum of all the primes below two million.

primes :: [Integer]
primes = 
    let helper x = if isPrime x then x : helper (x + 2) else helper (x + 2)
    in  2 : 3 : helper 5

isPrime :: Integer -> Bool
isPrime 1 = False
isPrime num =
    let limit = floor . sqrt . fromInteger $ num
    in  not $ any (\x -> num `mod` x == 0) [2..limit]

main = do
    putStrLn $ show $ sum $ takeWhile (<2000000) primes