-- A palindromic number reads the same both ways. The largest palindrome made from the product of two 2-digit numbers is 9009 = 91 × 99.
-- Find the largest palindrome made from the product of two 3-digit numbers.
-- Answer = 906609

calcValue :: Int -> Integer
calcValue digitSize =
    let h = (10 ^ digitSize) - 1
        l = 10 ^ (max (digitSize - 1) 1) 
    in  maximum $ filter isPalendrome [x * y | x <- [l..h], y <- [l..h]]

isPalendrome :: Show a => a -> Bool
isPalendrome val = (show val) == ((reverse . show) val)

main = do
    putStrLn $ show $ calcValue 3