-- 2^15 = 32768 and the sum of its digits is 3 + 2 + 7 + 6 + 8 = 26.

-- What is the sum of the digits of the number 2^1000?

import Data.Char (digitToInt)

calcValue :: Integer -> Integer
calcValue num =
    sum $ map (fromIntegral . digitToInt) $ show $ 2 ^ num

main = do
    putStrLn $ show $ calcValue 1000