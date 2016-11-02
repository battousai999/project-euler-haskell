-- Let d(n) be defined as the sum of proper divisors of n (numbers less than n which divide evenly into n).
-- If d(a) = b and d(b) = a, where a ≠ b, then a and b are an amicable pair and each of a and b are called amicable numbers.

-- For example, the proper divisors of 220 are 1, 2, 4, 5, 10, 11, 20, 22, 44, 55 and 110; therefore d(220) = 284. The proper divisors of 284 are 1, 2, 4, 71 and 142; so d(284) = 220.

-- Evaluate the sum of all the amicable numbers under 10000.

factors :: Integer -> [Integer]
factors num =
    let limit = (ceiling . sqrt . fromIntegral) num 
        partialFactors = filter (\x -> num `mod` x == 0 && x /= num) [1..limit]
    in  concat $ map (\x -> if num `div` x == x || x == 1 then [x] else [x, num `div` x]) partialFactors

isAmicable :: Integer -> Bool
isAmicable num =
    let sum1 = sum $ factors num
        sum2 = sum $ factors sum1
    in  sum2 == num && sum1 /= num

calcValue :: Integer -> Integer
calcValue num = sum $ filter isAmicable [1 .. (num - 1)]

main = do
    putStrLn $ show $ calcValue 10000

