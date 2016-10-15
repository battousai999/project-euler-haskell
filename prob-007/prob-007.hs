-- By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13.
-- What is the 10 001st prime number?

nthPrime :: Integer -> Integer
nthPrime num =
    let primeHelper cand counter =
            let isPrime' = isPrime cand
            in  if isPrime' && counter == num then cand
                else if isPrime' then primeHelper (cand + 1) (counter + 1)
                else primeHelper (cand + 1) counter
    in  primeHelper 1 1

isPrime :: Integer -> Bool
isPrime 1 = False
isPrime num =
    let limit = floor . sqrt . fromInteger $ num
    in  not $ any (\x -> num `mod` x == 0) [2..limit]


main = do
    putStrLn $ show $ nthPrime 10001
