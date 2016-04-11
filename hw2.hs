--Hayden Platt - HW2

-- 1. myFoldl function - meant to behave just like Haskell's built-in foldl
myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl op acc [] = acc
myFoldl op acc (x:xs) = myFoldl op(acc `op` x) xs

-- 2. myReverse - mimics Haskell's built-in reverse function
myReverse :: [a] -> [a]
myReverse xs = foldl (\a x -> x:a) [] xs

-- 3. myFoldr - Mimics Haskell's built-in foldr function using foldl
myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr op acc xs = foldl (\g x a -> g(op x a)) id xs acc

-- 4. myFoldl2 - Mimics Haskell's built-in foldl function using foldr
myFoldl2 :: (a -> b -> a) -> a -> [b] -> a
myFoldl2 op acc xs = foldr (\x g a -> g(op a x)) id xs acc

-- 5. isUpper - function that returns true if the provided character is in the range 'A' to 'Z'
isUpper :: Char -> Bool
isUpper c = c `elem` ['A'..'Z']

-- 6. onlyCapitals1 - Uses 'filter' and isUpper to return only the uppercase letters in a given string
onlyCapitals1 :: String -> String
onlyCapitals1 s = filter isUpper s

-- 7. onlyCapitals2 - Uses list comprehension and isUpper to return only the uppercase letters in a 
-- given string
onlyCapitals2 :: String -> String
onlyCapitals2 xs = [x | x <- xs, isUpper x]

-- 8. onlyCapitals3 - Uses pattern matching, guards, and isUpper to return only the 
-- uppercase letters in a given string
onlyCapitals3 :: String -> String
onlyCapitals3 [] = []
onlyCapitals3 (x:xs)
    | isUpper x == True = x : (onlyCapitals3 xs)
    | otherwise = onlyCapitals3 xs

--9. divRemainder - Returns a tuple with the quotient and the remainder of an integer division of the 
-- provided two numbers using 'div' and 'mod'
divRemainder :: Int -> Int -> (Int, Int)
divRemainder 0 0 = (0,0)
divRemainder x y = (x `div` y, x `mod` y)

--10. digitSum - Returns the sum of the digits of the given integer using recursion and guards
digitSum :: Int -> Int
digitSum x
    | x == 0 = 0
    | otherwise = x `mod` 10 + digitSum(x `div` 10)

--11. sayNum - Takes a string of digits and spells out the number as a string in English


--sayNum helper function - Returns the corresponding string name for an int in the ones' place
onesPlace :: Int -> String
onesPlace i = ["", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"] !! i

--sayNum helper function - Returns the corresponding string name for an int in the tens' place with a value less than 20
teensPlace :: Int -> String
teensPlace i = ["ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen"] !! i

--sayNum helper function - Returns the corresponding string name for an int in the tens' place with a value greater than 19
tensPlace :: Int -> String
tensPlace i = ["", "ten", "twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety"] !! i

--sayNum helper function - Returns the corresponding string name for an int in the a place with a value greater than 99
hugePlace :: Int ->String
hugePlace i =  ["", "thousand", "million", "billion", "trillion", "quadrillion", "quinitillion", "sextillion", "septillion", "octillion", "nonillion", "decillion", "undecillion", "duodecillion", "tredcillion", "quattuordecillion", "quindecillion", "sexdecillion", "septendecillion", "octodecillion", "novemdecillion", "vigintillion"] !! i

--sayNum helper function - Returns the string name for a 3-digit number
pronounceTriple :: Int -> String
pronounceTriple i = (if (i `div` 100) >=1
                                 then onesPlace (i `div` 100) ++ " hundred" --Determine how many hundred
                                 else "")
                                 ++
                             if ((i `mod` 100) `div` 10) == 1
                                 then " " ++ teensPlace(i `mod` 10) --Determine what teen number is correct
                                 else " " ++  tensPlace((i `mod` 100) `div` 10) ++ " " ++ onesPlace (i `mod` 10) --otherwise use tensPlace and OnesPlace for last 2 digits

--sayNum base function - only works on numbers up to 3 digits
sayNum :: String -> String
sayNum "0" = "zero"
sayNum x = unwords(words(pronounceTriple(read x :: Int)))