-- The prime factors of 13195 are 5, 7, 13 and 29.
-- What is the largest prime factor of the number 600851475143 ?
import Data.List (nub)

calcValue :: Integer -> Integer
calcValue num = 
    let groupedFactors = expand $ factor num
        factors = nub $ concatMap (\(a,b) -> a : [b]) groupedFactors 
    in  maximum factors

expand :: (Integer, Integer) -> [(Integer, Integer)]
expand (1, x) = [(1, x)]
expand (x, 1) = [(x, 1)]
expand (a, b) = expand (factor a) ++ expand (factor b)

-- Fermat factorization
factor :: Integer -> (Integer, Integer)
factor num =
    let val = ceiling $ sqrt $ fromIntegral num
        cGenerate i = ((val + i) * (val + i) - num) : cGenerate (i + 1) 
        b = head $ dropWhile (not . isSquare . fromIntegral) $ cGenerate 0 
        bs = ceiling $ sqrt $ fromIntegral b
        as = ceiling $ sqrt $ abs $ fromIntegral (b + num) 
    in  (as - bs, as + bs)

isSquare :: Integer -> Bool
isSquare a =
    let c = sqrt $ fromIntegral a 
    in  ceiling c == floor c

main = print $ calcValue 600851475143