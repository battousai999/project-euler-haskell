-- 2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.
-- What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?
-- Answer = 232792560

calcValue :: Integer -> Integer
calcValue num =
    let isDivisible x y = x `mod` y == 0 
        areAnyNotDivisible x = any (not . (isDivisible x)) [2..num] 
    in  head $ dropWhile areAnyNotDivisible [1..]

main = do
    putStrLn $ show $ calcValue 20

