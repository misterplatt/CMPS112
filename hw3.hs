--Hayden Platt - HW3

import Data.List

data BST k v = Empty |
               Node k v (BST k v) (BST k v)

--1. val - returns Just the stored value at the root node of the tree.
val :: BST k v -> Maybe v
val Empty = Nothing
val (Node _ x _ _) = Just x

--2. size - returns the number of nodes in the tree
size :: BST k v -> Int
size Empty = 0
size (Node _ _ left Empty) = 1 + (size left)
size (Node _ _ Empty right) = 1 + (size right)
size (Node _ _ left right) = 1 + (size left) + (size right)

--3. ins - inserts or updates a value v using k as a key
ins :: (Ord k) => k -> v -> BST k v -> BST k v
ins key val Empty = Node key val Empty Empty
ins key val (Node x y left right)
    | key == x = Node x val left right
    | key < x = Node x y (ins key val left) right
    | key > x = Node x y left (ins key val right)
    
--4. Making BST an instance of the Show type class
instance (Show v) => Show (BST k v) where
  show Empty = ""
  show (Node k v left right) = "(" ++ show left ++ show v ++ show right ++ ")"
  
--5. Making JSON an instance of the Show typeclass
data JSON = JStr String
          | JNum Double
          | JArr [JSON]
          | JObj [(String, JSON)]
          
instance Show JSON where
  show j = convertJSON j
  
-- Helper function which converts JStr, JNum, and JArr types to strings, and intercalates JObj's with commas
convertJSON :: JSON -> String
convertJSON (JStr js) = show js
convertJSON (JNum js) = show js
convertJSON (JArr js) = show js
convertJSON (JObj js) = "{" ++ intercalate "," (map (\(x, y) -> show x ++ ":" ++ convertJSON y) js) ++ "}" 

--6. Converting doubles and lists of json-things to and from Json
class Json a where
  toJson :: a -> JSON
  fromJson :: JSON -> a
  
instance Json Double where
  toJson = JNum
  fromJson = extractDouble
  
--Brief helper function which extracts the double value from a JNum
extractDouble :: JSON -> Double
extractDouble (JNum jn) = jn

instance (Json a) => Json [a] where
  toJson a = (JArr (map toJson a))
  fromJson (JArr a) = (map fromJson a)