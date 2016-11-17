-- The decimal number, 585 = 1001001001 (binary), is palindromic in both bases.

-- Find the sum of all numbers, less than one million, which are palindromic in base 10 and base 2.

-- (Please note that the palindromic number, in either base, may not include leading zeros.)

import Data.Digits (digits, unDigits)

convertBase :: Integral a => a -> a -> [a] -> [a]
convertBase from to = digits to . unDigits from

calcValue :: Integral a => a -> a
calcValue limit =
    let isDualPalindromic x = 
            let b10 = digits 10 x
                b2 = convertBase 10 2 b10
                isPalindrome xs = xs == (reverse xs)
            in  isPalindrome b10 && isPalindrome b2
    in  sum $ filter isDualPalindromic [1..fromIntegral (limit - 1)]

main = do
    putStrLn $ show $ calcValue 1000000
