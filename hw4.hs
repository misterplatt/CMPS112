--Hayden Platt - HW4

{-# LANGUAGE FlexibleInstances, OverlappingInstances, UndecidableInstances #-}
import System.Random
import Data.Int

--1. Gen Typeclass, now including instances for pairs and lists
class (Show a) => Gen a where
  gen :: IO a

instance (Show a, Random a) => Gen a where
  gen = randomIO
  
--For generating pairs
instance (Gen a, Gen b) => Gen (a, b) where
  gen = do
            x <- gen
            y <- gen
            return (x, y)
   
--For generating lists
instance (Gen a) => Gen [a] where
  gen = do
            len <- randomRIO(1,10) :: IO Int
            list <- genList len []
            return list
            
--List generating helper function. Takes in a length value and the original list to generate onto.
--While length isn't zero generates a new value and appends it to the list.
genList :: (Gen a) => Int -> [a] -> IO [a]
genList 0 list = return list
genList len list = do
                        n <- gen
                        genList (len - 1) (n : list)

--2. Testable typeclass used to test predicates. 
class Testable a where
  test :: String -> a -> IO (Bool, String)

instance Testable Bool where
  test s b = return (b, s)
  
--Takes in a string and a test, generates a random number, and returns the input string along with
--whether or not the test succeeded.
instance (Gen a, Testable b) => Testable (a -> b) where
  test s t = do 
              n <- gen
              s <- return (s ++ " " ++ show n)
              test s (t n)
              
--3/4. quickCheck - performs up to n tests with random inputs, repeatedly calling test until a failing test is encountered,
-- at which point it will print an error along with the failing inputs, and stop testing.
quickCheck :: (Testable a) => Int -> a -> IO ()
quickCheck 0 _ = return()
quickCheck n t = do
                          (x,y) <- test "" t
                          if x == False then putStrLn ("Failing Inputs = " ++ (show y))
                          else quickCheck (n-1) t
            
--5. 
isort :: [Int8] -> [Int8]
isort [] = []
isort (x:xs) = insert (isort xs)
  where insert [] = [x]
        insert (h:t) | x > h = h:insert t
                          | x <= h = x:h:t --Changed h:x:t to x:h:t so that the possibly lesser value x goes before h

qsort :: [Int8] -> [Int8]
qsort [] = []
qsort (x:xs) = qsort [a | a <- xs, a <= x] ++ [x] ++ qsort [a | a <- xs, a > x] --Changed a < x to a <= x so that repeats were not removed

testSort :: ([Int8] -> [Int8]) -> [Int8] -> Bool
testSort sort [] = True
testSort sort lst = length sortedList == length lst && isIncreasing sortedList && sort (sort lst) == sortedList
                            where sortedList = sort lst

--Helper function for testSort. Returns True if the values in a list are increasing, and false otherwise                            
isIncreasing :: [Int8] -> Bool
isIncreasing (x:y:ys) = x <=y && isIncreasing(y:ys)
isIncreasing _ = True
