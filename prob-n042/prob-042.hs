-- The nth term of the sequence of triangle numbers is given by, t_n = (1/2)n(n+1); so the first 
-- ten triangle numbers are:

-- 1, 3, 6, 10, 15, 21, 28, 36, 45, 55, ...

-- By converting each letter in a word to a number corresponding to its alphabetical position and 
-- adding these values we form a word value. For example, the word value for SKY is 19 + 11 + 25 = 55 = t_10. 
-- If the word value is a triangle number then we shall call the word a triangle word.

-- Using words.txt (right click and 'Save Link/Target As...'), a 16K text file containing nearly two-thousand 
-- common English words, how many are triangle words?

import Data.Char (toLower)
import Data.List (find)
import Data.List.Split (splitOn)
import System.IO (readFile)
import qualified Data.Text as T (replace, pack, unpack) 

triangleNumbers :: [Integer]
triangleNumbers = map (\x -> (x * (x + 1)) `div` 2) [1..]

isTriangleNumber :: Integer -> Bool
isTriangleNumber num 
    | num <= 0 = False
    | otherwise = 
        let result = find (>= num) triangleNumbers
        in  case result of
                Nothing -> False
                Just x  -> (x == num)

scoreWord :: String -> Integer
scoreWord word = sum $ map (\x -> fromIntegral $ (fromEnum $ toLower x) - 96) word

deQuote :: String -> String
deQuote = T.unpack . (T.replace (T.pack "\"") (T.pack "")) . T.pack

main = do
    rawData <- readFile "words.txt"
    let ws = map deQuote $ splitOn "," rawData
    putStrLn $ show $ length $ filter (isTriangleNumber . scoreWord) ws
