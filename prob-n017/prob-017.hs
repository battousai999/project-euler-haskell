-- If the numbers 1 to 5 are written out in words: one, two, three, four, five, then there are 3 + 3 + 5 + 4 + 4 = 19 letters used in total.

-- If all the numbers from 1 to 1000 (one thousand) inclusive were written out in words, how many letters would be used?

-- NOTE: Do not count spaces or hyphens. For example, 342 (three hundred and forty-two) contains 23 letters and 115 (one hundred and fifteen) contains 20 letters. The use of "and" when writing out numbers is in compliance with British usage.

wordifyNum :: Int -> String
wordifyNum num =
    let hundreds = num `div` 100
        tens = num `mod` 100
        onesWord x = 
            case x of
                0 -> ""
                1 -> "one"
                2 -> "two"
                3 -> "three"
                4 -> "four"
                5 -> "five"
                6 -> "six"
                7 -> "seven"
                8 -> "eight"
                9 -> "nine"
        condJoin con x y 
            | x == "" || y == "" = x ++ y
            | otherwise = x ++ con ++ y
        tensPhrase x 
            | x < 10 = onesWord x
            | x == 10 = "ten"
            | x == 11 = "eleven"
            | x == 12 = "twelve"
            | x == 13 = "thirteen"
            | x == 14 = "fourteen"
            | x == 15 = "fifteen"
            | x == 16 = "sixteen"
            | x == 17 = "seventeen"
            | x == 18 = "eighteen"
            | x == 19 = "nineteen"
            | x `div` 10 == 2 = condJoin "-" "twenty" $ onesWord (x `mod` 10)
            | x `div` 10 == 3 = condJoin "-" "thirty" $ onesWord (x `mod` 10)
            | x `div` 10 == 4 = condJoin "-" "forty" $ onesWord (x `mod` 10)
            | x `div` 10 == 5 = condJoin "-" "fifty" $ onesWord (x `mod` 10)
            | x `div` 10 == 6 = condJoin "-" "sixty" $ onesWord (x `mod` 10)
            | x `div` 10 == 7 = condJoin "-" "seventy" $ onesWord (x `mod` 10)
            | x `div` 10 == 8 = condJoin "-" "eighty" $ onesWord (x `mod` 10)
            | x `div` 10 == 9 = condJoin "-" "ninety" $ onesWord (x `mod` 10)
        hundredsPhrase x
            | x == 0 = ""
            | otherwise = (onesWord x) ++ " hundred"
    in  if num == 1000 then "one thousand" else condJoin " and " (hundredsPhrase hundreds) (tensPhrase tens)  
            
calcValue :: Int -> Int
calcValue num =
    let wordFilter = filter (\x -> x /= ' ' && x /= '-')
    in  sum $ map (length . wordFilter . wordifyNum) [1..num]

main = do
    putStrLn $ show $ calcValue 1000                
