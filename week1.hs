module ValidateCC where

--toDigits should convert positive Integers to a list of digits. (For 0 or
--negative inputs, toDigits should return the empty list.) toDigitsRev
--should do the same, but with the digits reversed.

toDigitsRev    :: Integer -> [Integer]
toDigitsRev n 
    | n <= 0 = []
    | otherwise = (n `mod` 10) : (toDigitsRev (n `div` 10))


toDigits :: Integer -> [Integer]
toDigits n = reverse (toDigitsRev n)

--Double the value of every second digit beginning from the right.
--That is, the last digit is unchanged; the second-to-last digit is doubled;
--the third-to-last digit is unchanged; and so on. For example,
--[1,3,8,6] becomes [2,3,16,6].

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther (n:x:ns) 
    | length(n:x:ns) `mod` 2 == 0 = n : (x*2) : [] ++ doubleEveryOther (ns)
    | otherwise =  doubleEveryOther((n:x:ns) ++ [0])

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (n:ns) = sum (toDigitsRev n) + (sumDigits (ns))

validate :: Integer -> Bool
validate = (== 0) . (`mod` 10) . sumDigits . doubleEveryOther . toDigitsRev
