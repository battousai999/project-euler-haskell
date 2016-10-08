calcValue :: Integer -> Integer
calcValue below = sum $ filter (\x -> x `mod` 3 == 0 || x `mod` 5 == 0) $ takeWhile (< below) [1..]

main = do
    putStrLn $ show $ calcValue 1000