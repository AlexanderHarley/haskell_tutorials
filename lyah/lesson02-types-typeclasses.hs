-- removeNonUppercase :: [Char] -> [Char]
removeNonUppercase :: String -> String
removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z'] ]

-- The last type is the return, the others are the parameters
-- Could think of it like Int, Int, Int -> Int
addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z

-- show takes a value whose type is a member of Show and presents as a string
show 3
-- "3"
show True
-- "True"

-- read is the opposite of show. Takes a string and returns a type which member of read
read "True" || False
-- True
read "8.2" + 3.8
-- 12.0
read "[1,2,3,4]" ++ [3]
-- [1,2,3,4,3]

-- read only works with 2 types (String + 1 other) - otherwise won't know what type to return
-- unless explicit type annotations are used
-- Can be done by adding :: at the end of the expression
read "5" :: Int
-- 5
read "[1,2,3,4]" :: [Int]
-- [1,2,3,4]
read "(3, 'a')" :: (Int, Char)
-- (3, 'a')

-- Enum
['a'..'e']
-- "abcde"

-- Bounded
minBound :: Int
-- -2147483648
maxBound :: Char
-- '\1114111'
maxBound :: (Bool, Int, Char)
-- (True,2147483647,'\1114111')
