-- A Pythagorean triplet is a set of three natural numbers, a < b < c, for which,

-- a^2 + b^2 = c^2
-- For example, 3^2 + 4^2 = 9 + 16 = 25 = 5^2.

-- There exists exactly one Pythagorean triplet for which a + b + c = 1000.
-- Find the product abc.

pythagTriplet :: Integer -> [(Integer, Integer, Integer)]
pythagTriplet num =
    [(a, b, c) | a <- [1..num], b <- [1..num], c <- [1..num], 
        a + b + c == num, 
        (square a) + (square b) == (square c),
        a < b && b < c]

square :: Num a => a -> a
square x = x * x

toList :: (a, a, a) -> [a]
toList (x, y, z) = x : y : [z]

main = do
    putStrLn $ show $ product . toList . head $ pythagTriplet 1000