-- The sum of the squares of the first ten natural numbers is,
--     1^2 + 2^2 + ... + 10^2 = 385
-- The square of the sum of the first ten natural numbers is,
--     (1 + 2 + ... + 10)^2 = 55^2 = 3025
-- Hence the difference between the sum of the squares of the first ten natural numbers and the square of the sum is 3025 − 385 = 2640.
-- Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum.

calcValue :: Integer -> Integer
calcValue num =
    let sumOfSquares = sum $ map square [1..num]
        squareOfSums = square $ sum [1..num]
    in  abs (squareOfSums - sumOfSquares)

square :: Num a => a -> a
square num = num * num

main = do
    putStrLn $ show $ calcValue 100