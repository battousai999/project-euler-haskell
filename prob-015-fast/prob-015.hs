-- Starting in the top left corner of a 2×2 grid, and only being able to move to the right and down, there are exactly 6 routes to the bottom right corner.

-- How many such routes are there through a 20×20 grid?

choose :: Integral a => a -> a -> a
n `choose` k
  | k > n           = undefined
  | k == 0          = 1
  | k > (n `div` 2) = n `choose` (n-k)
  | otherwise       = n * ((n-1) `choose` (k-1)) `div` k

calcValue :: Integral a => a -> a
calcValue num = (2 * num) `choose` num

main = do
    putStrLn $ show $ calcValue 20