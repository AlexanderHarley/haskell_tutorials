doubleMe x = x + x
-- doubleUs x y = x*2 + y*2
doubleUs x y = doubleMe x + doubleMe y
doubleSmallNumber x = if x > 100
                        then x
                        else x*2
doubleSmallNumber' x = (if x > 100 then x else x*2) + 1

-- Add 2 lists (cannot contain mixed numbers/letters)
firstNumbers = [1,4,7,23,45,50]
secondNumbers = [9,10,11,12]
bothNumbers = firstNumbers ++ secondNumbers

-- (++) Add to end of list (HAS TO LOOP THROUGH ENTIRE LIST)
firstNumbers ++ [100]

-- (:)  Add to start of list (Better way)
100:firstNumbers

-- (!!) Get number from position in list
firstNumbers !! 0
-- get first element
head firstNumbers
-- get all but first element
tail firstNumbers
-- get last element
last firstNumbers
-- get all but last element
init firstNumbers

-- Compare numbers in lists (starts at index [0])
[3,2,1] > [2,1,0]

-- length of list
length firstNumbers

-- check if list is empty
null firstNumbers

-- reverse list
reverse firstNumbers

-- extract x elements from start list (list[x])
take 3 firstNumbers

-- same as above but drops from start of list
drop 2 firstNumbers

-- check if number is in list
4 `elem` firstNumbers

-- enumerate list
[1..5]
-- returns [1,2,3,4,5]

-- enumerate list with step
[2,4..10]
-- returns [2,4,6,8,10]

-- enumerate list backwards requires 2 numbers at start
[5,4..1]
-- returns [5,4,3,2,1]

-- enumerate characters
['a'..'z']
-- returns "abcdefghijklmnopqrstuvwxyz"

-- you can also make infinite lists by giving no upper limit
take 24 [13,26..]
-- returns first 24 elements of list only, lazy evaluation

-- cycle creates an infinite list from another list
take 10 (cycle [1,2,3])
-- returns [1,2,3,1,2,3,1,2,3,1]
take 12 (cycle "LOL ")
-- returns "LOL LOL LOL "

-- repeat does the same, but with a list with only 1 element
take 4 (repeat 5)
-- returns [5,5,5,5]

-- replicate
replicate 3 10
-- returns [10,10,10]


---------------------
-- List Comprehension
---------------------
[x*2 | x <- [1..10]]
-- [2,4,6,8,10,12,14,16,18,20]

-- same as:
take 10 [2,4..]
-- [2,4,6,8,10,12,14,16,18,20]

-- add a predicate to filter results, 'filtering'
[x*2 | x <- [1..10], x*2 >= 10]
-- [10,12,14,16,18,20]

boomBangs xs = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x ]
boomBangs [7..13]
-- ["BOOM!","BOOM!","BANG!","BANG!"]
boomBangs' xs = [ if even x then "BOOM!" else "BANG!" | x <- xs ]
boomBangs' [10..15]
-- ["BOOM!","BANG!","BOOM!","BANG!","BOOM!","BANG!"]

-- can include multiple predicates
-- all numbers between 10 to 20 that aren't 13, 15, 19
[ x | x <- [10..20], x /= 13, x /= 15, x /= 19 ]
-- [10,11,12,14,16,17,18,20]

-- draw from multiple lists
[ x*y | x <- [2,5,10], y <- [8,10,11] ]
-- [16,20,22,40,50,55,80,100,110]

multiplyLists x y = [ x*y | x <- x, y <- y ]
multiplyLists [2,5,10] [8,10,11]
-- [16,20,22,40,50,55,80,100,110]

-- filter out only numbers greater than 50
multiplyLists' x y = [ x*y | x <- x, y <- y, x*y > 50 ]
multiplyLists' [2,5,10] [8,10,11]
-- [55,80,100,110]

nouns = ["pythonista", "rustacean", "gopher"]
adjectives = ["lazy", "bright", "mega"]
[ adjective ++ " " ++ noun | adjective <- adjectives, noun <- nouns ]
-- ["lazy pythonista","lazy rustacean","lazy gopher","bright pythonista" etc.]

nouns' = ["Javascript", "PHP", "C"]
titles' = ["Artisan", "Expert", "Legend"]
coderMemes x y = [ noun ++ " " ++ title | noun <- x, title <- y, title == "Artisan"]
-- ["Javascript Artisan", "PHP Artisan", "C Artisan"]

-- '_' can be used as a throwaway variable
-- This function replaces every element of a list with 1 then sums that up
length' xs = sum [1 | _ <- xs]

-- List comprehension to process and produce strings example
removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z'] ]
removeNonUppercase "IdontLIKEFROGS"
-- "ILIKEFROGS"


---------------------
-- Tuples
---------------------
-- Tuples can contain multiple types (doesn't need to be homogenous)
-- Tuples in a list must be all the same length

-- e.g. [(1,2),(8,11),(4,5)] all contain 2, correct
--      [(1,2),("One", 2)] also fine, correct
-- but [(1,2),(8,11,5),(4,5)] would throw an error

-- you can only compare 2 tuples of the same length

-- fst takes a PAIR (2 tuple only) and returns it's first component
fst (8,11)
-- 8
fst ("Wow", False)
-- "Wow"

-- snd takes a PAIR (2 tuple only) and returns it's second component
snd (8,11)
-- 11
snd ("Wow", False)
-- False

-- zip combines matching elements from lists into pairs
zip [1,2,3,4,5] [5,5,5,5,5]
-- [(1,5),(2,5),(3,5),(4,5),(5,5)]
zip [1..5] ["one", "two", "three", "four", "five"]
-- [(1,"one"),(2,"two"),(3,"three"),(4,"four"),(5,"five")]
zip [1..] ["apple", "orange", "cherry", "mango"]
-- [(1,"apple"),(2,"orange"),(3,"cherry"),(4,"mango")]

triangles = [ (a,b,c) | c <- [1..10], b <- [1..10], a <- [1..10] ]
rightTriangles = [ (a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2 ]
rightTriangles' = [ (a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2, a+b+c == 24]
rightTriangles'
-- [(6,8,10)]
