-- An irrational decimal fraction is created by concatenating the positive integers:

-- 0.123456789101112131415161718192021...

-- It can be seen that the 12th digit of the fractional part is 1.

-- If d_n represents the nth digit of the fractional part, find the value of the following expression.

-- d_1 × d_10 × d_100 × d_1000 × d_10000 × d_100000 × d_1000000

calcValue :: [Int] -> Int
calcValue indexes =
    let digits = foldr (++) "" $ map show [1..]
        indexedDigits = map (\x -> read $ [digits !! (x - 1)]) indexes
    in  product indexedDigits

main = do
    putStrLn $ show $ calcValue [1, 10, 100, 1000, 10000, 100000, 1000000]
    