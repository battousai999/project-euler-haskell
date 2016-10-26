-- Starting in the top left corner of a 2×2 grid, and only being able to move to the right and down, there are exactly 6 routes to the bottom right corner.

-- How many such routes are there through a 20×20 grid?

import Data.Function.Memoize (memoFix)

data Tree a = Node a (Tree a) (Tree a)
    | Empty
    deriving Show

buildMoves :: Int -> Tree (Int, Int)
buildMoves size = 
    let buildMovesHelper f (xPos, yPos)
            | xPos == size && yPos == size = Node (xPos, yPos) Empty Empty
            | xPos == size                 = Node (xPos, yPos) Empty (f (xPos, yPos + 1))
            | yPos == size                 = Node (xPos, yPos) (f (xPos + 1, yPos)) Empty
            | otherwise                    = Node (xPos, yPos) (f (xPos + 1, yPos)) (f (xPos, yPos + 1))
        helper = memoFix buildMovesHelper
    in  helper (0,0)

countLeafNodes :: Tree a -> Integer
countLeafNodes (Node _ Empty Empty) = 1
countLeafNodes (Node _ Empty r)     = countLeafNodes r
countLeafNodes (Node _ l Empty)     = countLeafNodes l
countLeafNodes (Node _ l r)         = (countLeafNodes l) + (countLeafNodes r)

main = do
    putStrLn $ show $ countLeafNodes $ buildMoves 20            
