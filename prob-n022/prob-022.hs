-- Using names.txt (right click and 'Save Link/Target As...'), a 46K text file containing over five-thousand first names, begin by sorting it into alphabetical order. Then working out the alphabetical value for each name, multiply this value by its alphabetical position in the list to obtain a name score.

-- For example, when the list is sorted into alphabetical order, COLIN, which is worth 3 + 15 + 12 + 9 + 14 = 53, is the 938th name in the list. So, COLIN would obtain a score of 938 × 53 = 49714.

-- What is the total of all the name scores in the file?

import System.IO (readFile)
import Data.Char (toLower)
import Data.List.Split (splitOn)
import qualified Data.Text as T (replace, unpack, pack)

quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let less = filter (<x) xs
        more = filter (>x) xs
    in  (quicksort less) ++ [x] ++ (quicksort more)

alphaValue :: String -> Integer
alphaValue str = 
    let charValue c = ((fromIntegral . fromEnum . toLower) c) - 96
    in  sum $ map charValue str

deQuote :: String -> String
deQuote = T.unpack . (T.replace (T.pack "\"") (T.pack "")) . T.pack

main = do
    rawData <- readFile "names.txt"
    let names = map deQuote $ splitOn "," rawData
    let sortedNames = zip [1..] $ quicksort names
    let results = sum $ map (\x -> (fst x) * ((alphaValue . snd) x)) sortedNames
    putStrLn $ show results
