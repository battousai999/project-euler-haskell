-- A perfect number is a number for which the sum of its proper divisors is exactly equal to the number. For example, the sum of the proper divisors of 28 would be 1 + 2 + 4 + 7 + 14 = 28, which means that 28 is a perfect number.

-- A number n is called deficient if the sum of its proper divisors is less than n and it is called abundant if this sum exceeds n.

-- As 12 is the smallest abundant number, 1 + 2 + 3 + 4 + 6 = 16, the smallest number that can be written as the sum of two abundant numbers is 24. By mathematical analysis, it can be shown that all integers greater than 28123 can be written as the sum of two abundant numbers. However, this upper limit cannot be reduced any further by analysis even though it is known that the greatest number that cannot be expressed as the sum of two abundant numbers is less than this limit.

-- Find the sum of all the positive integers which cannot be written as the sum of two abundant numbers.

smallestAbundantNum = 12
largestUnprovableAbundantSum = 28123

factors :: Integer -> [Integer]
factors num =
    let limit = (ceiling . sqrt . fromIntegral) num 
        partialFactors = filter (\x -> num `mod` x == 0 && x /= num) [1..limit]
        factorFilter x 
            | x == limit && num `div` x == limit - 1 = []
            | num `div` x == x || x == 1             = [x]
            | otherwise                              = [x, num `div` x]
    in  concat $ map factorFilter partialFactors

abundantNumbers :: [Integer]
abundantNumbers =
    let isAbundant num = (sum $ factors num) > num
        helper x 
            | isAbundant x = x : helper (x + 1)
            | otherwise    = helper  (x + 1)
    in  smallestAbundantNum : helper (smallestAbundantNum + 1)

isSumOfAbundant :: Integer -> Bool
isSumOfAbundant num 
    | num < (2 * smallestAbundantNum)    = False
    | num == (2 * smallestAbundantNum)   = True
    | num > largestUnprovableAbundantSum = True
    | otherwise = 
        let candidates = takeWhile (<= (num - smallestAbundantNum)) abundantNumbers
            candidateSums = [x + y | x <- candidates, y <- candidates, x + y == num]
        in  not $ null candidateSums

calcValue :: Integer
calcValue = sum $ filter (not . isSumOfAbundant) [1..largestUnprovableAbundantSum]

main = do
    putStrLn $ show calcValue