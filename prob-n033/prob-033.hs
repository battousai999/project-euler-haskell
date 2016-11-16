-- The fraction 49/98 is a curious fraction, as an inexperienced mathematician in attempting to simplify it may 
-- incorrectly believe that 49/98 = 4/8, which is correct, is obtained by cancelling the 9s.

-- We shall consider fractions like, 30/50 = 3/5, to be trivial examples.

-- There are exactly four non-trivial examples of this type of fraction, less than one in value, and containing 
-- two digits in the numerator and denominator.

-- If the product of these four fractions is given in its lowest common terms, find the value of the denominator.

import Data.Ratio ((%), denominator)

removeDigit :: Int -> Int -> Int
removeDigit digit num =
    let cDigit = (head . show) digit
        sNum = show num
    in  (read :: String -> Int) $ filter (/= cDigit) sNum

curiousFractions :: [(Int, Int)]
curiousFractions =
    [(n, d) | 
        c <- [1..9],
        d <- concat $ [[(c * 10 + a), (a * 10 + c)] | a <- [1..9], a /= c],
        n <- filter (<d) $ concat $ [[(c * 10 + a), (a * 10 + c)] | a <- [1..9], a /= c],
        (n % d) == ((removeDigit c n) % (removeDigit c d))]

main = do
    let value = (denominator . product) $ map (\(n,d) -> n % d) curiousFractions
    putStrLn $ show $ value