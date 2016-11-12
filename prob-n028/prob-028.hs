-- Starting with the number 1 and moving to the right in a clockwise direction a 5 by 5 spiral is formed as follows:

-- 21 22 23 24 25
-- 20  7  8  9 10
-- 19  6  1  2 11
-- 18  5  4  3 12
-- 17 16 15 14 13

-- It can be verified that the sum of the numbers on the diagonals is 101.

-- What is the sum of the numbers on the diagonals in a 1001 by 1001 spiral formed in the same way?


-- Formulas for diagonals of Ulam's spiral

-- 4n^2+2n+1
-- 4n^2+1
-- 4n^2-2n+1
-- 4n^2-4n+1 -- this one starts with 1


diagonal_1 :: [Integer]
diagonal_1 = map (\n -> 4 * n * n + 2 * n + 1) [1..]

diagonal_2 :: [Integer]
diagonal_2 = map (\n -> 4 * n * n + 1) [1..]

diagonal_3 :: [Integer]
diagonal_3 = map (\n -> 4 * n * n - 2 * n + 1) [1..]

diagonal_4 :: [Integer]
diagonal_4 = map (\n -> 4 * n * n - 4 * n + 1) [1..]

sumDiagonals :: Int -> Integer
sumDiagonals size =
    let armSize = (size - 1) `div` 2
        s1 = sum $ take armSize diagonal_1
        s2 = sum $ take armSize diagonal_2
        s3 = sum $ take armSize diagonal_3
        s4 = sum $ take (armSize + 1) diagonal_4
    in  s1 + s2 + s3 + s4

main = do
    putStrLn $ show $ sumDiagonals 1001
