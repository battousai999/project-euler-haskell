-- Triangle, pentagonal, and hexagonal numbers are generated by the following formulae:

-- Triangle	 	T_n=n(n+1)/2	 	1, 3, 6, 10, 15, ...
-- Pentagonal	 	P_n=n(3n−1)/2	 	1, 5, 12, 22, 35, ...
-- Hexagonal	 	H_n=n(2n−1)	 	1, 6, 15, 28, 45, ...
-- It can be verified that T_285 = P_165 = H_143 = 40755.

-- Find the next triangle number that is also pentagonal and hexagonal.

triangleNumber :: Integer -> Integer
triangleNumber num = num * (num + 1) `div` 2

triangleNumbers :: [Integer]
triangleNumbers = map triangleNumber [1..]

pentagonalNumber :: Integer -> Integer
pentagonalNumber num = num * (3 * num - 1) `div` 2

pentagonalNumbers :: [Integer]
pentagonalNumbers = map pentagonalNumber [1..]

hexagonalNumber :: Integer -> Integer
hexagonalNumber num = num * (2 * num - 1)

hexagonalNumbers :: [Integer]
hexagonalNumbers = map hexagonalNumber [1..]

nextTripleAfter :: Integer -> Integer
nextTripleAfter num =
    let helper (h:hs) (p:ps) (t:ts)
            | h == p && p == t && h > num = h
            | h == p && p == t = helper hs ps ts
            | h > p && h > t = helper (h:hs) ps ts
            | h > p = helper (h:hs) ps (t:ts)
            | h > t = helper (h:hs) (p:ps) ts
            | otherwise = helper hs ps ts
    in  helper hexagonalNumbers pentagonalNumbers hexagonalNumbers

main = print $ nextTripleAfter 40755
