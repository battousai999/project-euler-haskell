-- In England the currency is made up of pound, £, and pence, p, and there are eight coins in general circulation:

-- 1p, 2p, 5p, 10p, 20p, 50p, £1 (100p) and £2 (200p).
-- It is possible to make £2 in the following way:

-- 1×£1 + 1×50p + 2×20p + 1×5p + 1×2p + 3×1p
-- How many different ways can £2 be made using any number of coins?

data Solution = Solution { 
    p1 :: Int,
    p2 :: Int,
    p5 :: Int,
    p10 :: Int,
    p20 :: Int,
    p50 :: Int,
    e1 :: Int,
    e2 :: Int
}

solutions :: Int -> [Solution]
solutions val =
    let totalP = val * 100
        pValue (Solution p1 p2 p5 p10 p20 p50 e1 e2) =
            p1 +
            p2 * 2 +
            p5 * 5 +
            p10 * 10 +
            p20 * 20 + 
            p50 * 50 +
            e1 * 100 +
            e2 * 200
        possibleSolutions = 
            [ (Solution v_p1 v_p2 v_p5 v_p10 v_p20 v_p50 v_e1 v_e2) |
                v_p1 <- [0..totalP],
                v_p2 <- [0..(totalP `div` 2)],
                v_p5 <- [0..(totalP `div` 5)],
                v_p10 <- [0..(totalP `div` 10)],
                v_p20 <- [0..(totalP `div` 20)],
                v_p50 <- [0..(totalP `div` 50)],
                v_e1 <- [0..val],
                v_e2 <- [0..(val `div` 2)]
            ]
    in  filter (\x -> pValue x == totalP) possibleSolutions 

main = do
    putStrLn $ show $ length $ solutions 2