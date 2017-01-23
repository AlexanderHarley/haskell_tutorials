-- Lots of stuff uncommented in this file, don't import without some cleanup

import Data.List

-- Import selected functions only:
import Data.List (nub, sort)

-- Import all functions except nub:
import Data.List hiding (nub)

-- Namespace import (same as python 'import modulename as Module')
import qualified Data.Map as M

numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub
-- i.e. numUniques = \xs -> length (nub xs)
-- or   numUniques x = length (nub x)
-- or   numUniques x = length $ nub x


-- ===================
-- Data.List Functions
-- ===================

-- intersperse takes an element and a list and puts that element in between each pair of elements in the list
intersperse '.' "MONKEY"
-- "M.O.N.K.E.Y"
intersperse 0 [1,2,3,4,5,6]
-- [1,0,2,0,3,0,4,0,5,0,6]

-- intercalate takes a list and a list of lists, inserts that list between each list, then flattens to 1 list
intercalate " " ["hey","there","guys"]
-- "hey there guys"
intercalate [0,0,0] [[1,2,3],[4,5,6],[7,8,9]]
-- [1,2,3,0,0,0,4,5,6,0,0,0,7,8,9]

-- transpose transposes a list of lists. Looking at a list of lists as a 2D matrix, the columns become the rows and vice versa
transpose [[1,2,3],[4,5,6],[7,8,9]]
-- [[1,4,7],[2,5,8],[3,6,9]]
transpose ["hey","there","guys"]
-- ["htg","ehu","yey","rs","e"]

-- concat flattens a list of lists into a single list of elements
concat ["foo","bar","car"]
-- "foobarcar"
concat [[3,4,5],[2,3,4],[2,1,1]]
-- [3,4,5,2,3,4,2,1,1]

-- concatMap is the same as first mapping a function ot a list then using 'concat' on the list
concatMap (replicate 4) [1..3]
-- [1,1,1,1,2,2,2,2,3,3,3,3]

-- 'and' takes a list of boolean values and returns True only if all the values in the list are True
and $ map (>4) [5,6,7,8]
-- True
and $ map (==4) [4,4,4,3,4]
-- False

-- 'or' is like 'and', but returns True if ANY of the boolean values in a list is True
or $ map (==4) [4,4,4,3,4]
-- True
or $ map (>4) [1,2,3]
-- False

-- 'all' and 'any' are the same as 'and' and 'or' respectively, only they take a predicate.
-- This saves mapping over a list with 'and' or 'or'.
all (==4) [4,4,4,3,4]
-- False
any (==4) [4,4,4,3,4]
-- True

-- iterate takes a function and a starting value. It applies the function to the starting value, then to the result etc.
-- It returns all the results in the form of an infinite list.
take 10 $ iterate (*2) 1
-- [1,2,4,8,16,32,64,128,256,512]
take 3 $ iterate (++ "haha") "haha"
-- ["haha","hahahaha","hahahahahaha"]
