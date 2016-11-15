-- In England the currency is made up of pound, £, and pence, p, and there are eight coins in general circulation:

-- 1p, 2p, 5p, 10p, 20p, 50p, £1 (100p) and £2 (200p).
-- It is possible to make £2 in the following way:

-- 1×£1 + 1×50p + 2×20p + 1×5p + 1×2p + 3×1p
-- How many different ways can £2 be made using any number of coins?

data Coin = P1 | P2 | P5 | P10 | P20 | P50 | E1 | E2
    deriving Show

coinValue :: Coin -> Int -> Int
coinValue c v = 
    case c of
        P1  -> v
        P2  -> 2 * v
        P5  -> 5 * v
        P10 -> 10 * v
        P20 -> 20 * v
        P50 -> 50 * v 
        E1  -> 100 * v
        E2  -> 200 * v

type CoinSet = (Coin, Int)
type Solution = [CoinSet]

combine :: CoinSet -> [Solution] -> [Solution]
combine c xs = map (\x -> c : x) xs

solutions :: Int -> [Solution]
solutions val =
    let totalP = val * 100
        solutions_abstract coin nextFunc innerValue =
            let candidates = [0..(innerValue `div` (coinValue coin 1))]
            in  concat $ map (\x -> combine (coin, x) $ nextFunc (innerValue - (coinValue coin x))) candidates
        solutions_e2 = solutions_abstract E2 solutions_e1
        solutions_e1 = solutions_abstract E1 solutions_p50
        solutions_p50 = solutions_abstract P50 solutions_p20
        solutions_p20 = solutions_abstract P20 solutions_p10
        solutions_p10 = solutions_abstract P10 solutions_p5
        solutions_p5 = solutions_abstract P5 solutions_p2
        solutions_p2 = solutions_abstract P2 solutions_p1
        solutions_p1 innerValue = [[(P1, innerValue)]]
    in  solutions_e2 totalP

main = do
    putStrLn $ show $ length $ solutions 2
