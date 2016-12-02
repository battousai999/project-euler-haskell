-- The series, 1^1 + 2^2 + 3^3 + ... + 10^10 = 10405071317.

-- Find the last ten digits of the series, 1^1 + 2^2 + 3^3 + ... + 1000^1000.

selfPowerSum :: Int -> Integer
selfPowerSum p = sum $ map (\x -> fromIntegral x ^ x) [1..p]

calcValue :: Int -> Int -> Integer
calcValue num digits = read $ reverse $ take digits $ reverse $ show $ selfPowerSum num

main = print $ calcValue 1000 10
