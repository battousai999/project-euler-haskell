calcValue :: Integer -> Integer
calcValue limit = sum $ filter even $ takeWhile (<= limit) fib 

fib :: [Integer]
fib = 
    let fibInner a b = (a + b) : fibInner b (a + b) in
        1 : 2 : fibInner 1 2

main = do
    putStrLn $ show $ calcValue 4000000