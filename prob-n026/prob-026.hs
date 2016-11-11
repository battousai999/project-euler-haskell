-- A unit fraction contains 1 in the numerator. The decimal representation of the unit fractions with denominators 2 to 10 are given:

-- 1/2	= 	0.5
-- 1/3	= 	0.(3)
-- 1/4	= 	0.25
-- 1/5	= 	0.2
-- 1/6	= 	0.1(6)
-- 1/7	= 	0.(142857)
-- 1/8	= 	0.125
-- 1/9	= 	0.(1)
-- 1/10	= 	0.1
-- Where 0.1(6) means 0.166666..., and has a 1-digit recurring cycle. It can be seen that 1/7 has a 6-digit recurring cycle.

-- Find the value of d < 1000 for which 1/d contains the longest recurring cycle in its decimal fraction part.

import Data.List (maximumBy)

multiplicativeOrder :: Integer -> Integer -> Integer
multiplicativeOrder num modNum =
    let f c = (num ^ c) `mod` modNum
        xs = filter (\x -> f x == 1) [1..modNum]
    in  if null xs then 0 else head xs

factorOut :: Integer -> Integer -> Integer
factorOut num factor 
    | num `mod` factor == 0 = factorOut (num `div` factor) factor
    | otherwise             = num

periodOfUnitFraction :: Integer -> Integer
periodOfUnitFraction num =
    let r = factorOut (factorOut num 2) 5
    in  multiplicativeOrder 10 r

calcValue :: Integer -> Integer
calcValue limit =
    let vals = map (\x -> (x, periodOfUnitFraction x)) [1..(limit -1)]
    in  fst $ maximumBy (\x y -> (snd x) `compare` (snd y)) vals

main = do
    (putStrLn . show) $ calcValue 1000
