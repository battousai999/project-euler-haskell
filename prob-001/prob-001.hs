calcValue :: Integer
calcValue = sum $ filter (\x -> x `mod` 3 == 0 || x `mod` 5 == 0) $ takeWhile (<1000) [1..]

main = do
    putStrLn $ show calcValue