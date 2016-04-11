--Hayden Platt - HW1

-- 1. citeAuthor function - Reverses the order of two strings
citeAuthor :: String -> String -> String
citeAuthor first last = last ++ ", " ++ first

-- 2. initials function - Cuts two string down to their first letter,
-- followed by periods.
initials :: String -> String -> String
initials first last = take 1 first ++ "." ++ take 1 last ++ "."

-- 3. title function - Extracts the 2nd element from a 3-tuple
title :: (String, String, Int) -> String
title (_, title, _) = title

-- 4. citeBook function - Formats a 3-tuple for book citation.
citeBook :: (String, String, Int) -> String
citeBook (author, title, year) = title ++ " (" ++ author ++ ", " ++ show year ++ ")"

-- 5. bibliography_rec function - Formats a list of 3-tuples for book citation.
bibliography_rec :: [(String, String, Int)] -> String
bibliography_rec [] = ""
bibliography_rec (x:xs) =  citeBook x ++ "\n" ++ (bibliography_rec xs)

-- 6. averageYear function - Averages the 3rd element from a list of 3-tuples
averageYear :: [(String, String, Int)] -> Int
averageYear [] = 0
averageYear xs = sum (map getYear xs) `div` length xs

--Helper function for averageYear. Extracts the 3rd element from a 3-tuple
getYear :: (String, String, Int) -> Int
getYear (_,_,year) = year

--Definition for questions 7 and 8
txt :: String
txt = "[1] and [2] both feature characters who will do whatever it takes to " ++
      "get to their goal, and in the end the thing they want the most ends " ++
      "up destroying them.  In case of [2] this is a whale..."

-- 7. references function - Counts the number of reference symbols ([#])
-- in a given text
references :: String -> Int
references text = length (filter myPredicate (words text))

--The predicate function of references. Used to filter out non-reference symbols.
myPredicate :: String -> Bool
myPredicate word
                   | take 1 word == "[" = True
                   | otherwise = False

-- 8. citeText function - Replaces any reference symbols ([#]) in a text with
-- a full citation.
citeText :: [(String, String, Int)] -> String -> String
citeText refs text = convertRefs refs (words text)

--The recursive helper function of citeText. Replaces reference symbols ([#]) with
-- the book citation at the appropriate index, and concatenates them among the text.
convertRefs :: [(String, String, Int)] -> [String] -> String
convertRefs refs [] = ""
convertRefs refs (x:xs) = if take 1 x == "[" 
                                     then citeBook (refs !! pullRefIndex x) ++ " " ++ convertRefs refs xs
                                     else x ++ " " ++ convertRefs refs xs

--The helper function of convertRefs. Returns the the feference number -1, 
-- AKA its index in the list
pullRefIndex :: String -> Int
pullRefIndex ref = read(tail(take 2 ref)) - 1