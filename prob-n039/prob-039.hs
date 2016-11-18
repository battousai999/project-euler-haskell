-- If p is the perimeter of a right angle triangle with integral length sides, {a,b,c}, 
-- there are exactly three solutions for p = 120.

-- {20,48,52}, {24,45,51}, {30,40,50}

-- For which value of p ≤ 1000, is the number of solutions maximised?

import Data.List (maximumBy)

type Triangle = (Int, Int, Int)

trianglesOfPerimeter :: Int -> [Triangle]
trianglesOfPerimeter p =
    let square x = x * x
    in  [(a, b, c) |
            c <- [1..(p - 2)],
            b <- [1..(c - 1)],
            let a = ceiling $ sqrt $ fromIntegral $ square c - square b,
            a + b + c == p,
            a < b,
            square a + square b == square c]

calcValue :: Int -> Int
calcValue limit = 
    let ps = map (\x -> (x, length $ trianglesOfPerimeter x)) [12..limit]
    in  fst $ maximumBy (\x y -> (snd x) `compare` (snd y)) ps

main = do
    putStrLn $ show $ calcValue 1000
