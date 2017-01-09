-- A higher order function is a function that takes functions as parameters and/or returns functions as return values

-- All Haskell functions take just 1 parameter which is a function that returns another function.
-- This is known as 'currying'
multiThree :: (Num a) => a -> a -> a -> a
multiThree x y z = x * y * z

-- multiThree 3 5 9
-- could be thought of as
-- ((multiThree 3) 5) 9
-- multiThree takes the parameter 3 and returns a function, which 5 is applied to, which returns a function with 15 as a parameter.

-- By calling functions with too few parameters allows creation of new functions on the fly
-- These are called 'partially applied' functions
multiTwoWithNine = multiThree 9
-- multiTwoWithNine 2 3
-- 54

compareWithHundred :: (Num a, Ord a) => a -> Ordering
compareWithHundred = compare 100

divideByTen :: (Floating a) => a -> a
divideByTen = (/10)

isUpperAlphaNum :: Char -> Bool
isUpperAlphaNum = (`elem` ['A'..'Z'])

applyTwice :: (a -> a) -> a -> a
-- (a -> a) -- parentheses indicate the first parameter is a function that takes something and returns the same thing
-- the second parameter is something of that type also and returns the same type
-- for the sake of simplicity, we'll say this function takes 2 parameters and returns 1 thing
applyTwice f x = f (f x)

-- applyTwice (multiThree 2 2) 9
-- .. = (2*2) * ((2*2)*9)
-- .. = 4 * (4*9)
-- 144

-- applyTwice (3:) [1]
-- [3,3,1]

-- If the type declaration of a function says it accepts an (a -> b -> c) function as a parameter,
-- it will also accept an (a -> a -> a) function, but not the other way around!
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

-- zipWith' (*) (replicate 5 2) [1..]
-- (*) [2,2,2,2,2] [1,2,3,4,5]
-- [2,4,6,8,10]

flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f = g
    where g x y = f y x

-- can also be defined as:

flip'' :: (a -> b -> c) -> b -> a -> c
flip'' f y x = f x y

-- flip' zip [1,2,3,4,5] "hello"
-- [('h',1),('e',2),('l',3),('l',4),('o',5)]


-- ================
-- Maps and filters
-- ================

-- map (+3) [1,5,3,1,6]
-- [4,8,6,4,9]
-- ^ same as doing list comprehension [x+3 | x <- [1,5,3,1,6]]

-- map (++ "!") ["BIFF", "BANG", "POW"]
-- ["BIFF!", "BANG!", "POW!"]

-- map (replicate 3) [3..6]
-- [[3,3,3],[4,4,4],[5,5,5],[6,6,6]]

-- Note: Filter doesn't work on infinite lists. takeWhile does.

-- filter (>3) [1,5,3,2,1,6,4,3,2,1]
-- [5,6,4]

-- filter (==3) [1,2,3,4,5]
-- [3]

-- filter even [1..10]
-- [2,4,6,8,10]

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let sSorted = quicksort (filter (<=x) xs)
        bSorted = quicksort (filter (>x) xs)
    in  sSorted ++ [x] ++ bSorted

largestDivisible :: (Integral a) => a
largestDivisible = head (filter p [100000,99999..])
    where p x = x `mod` 3829 == 0

-- takeWhile goes from the beginning of a list and returns elements while the predicate holds true
-- when the predicate doesn't hold, it stops:
-- takeWhile (/=' ') "elephants know how to party"
-- "elephants"

-- sum (takeWhile (<10000) (filter odd (map (^2) [1..])))
-- 166650

-- sum (takeWhile (<10000) [n^2 | n <- [1..], odd (n^2)])
-- 166650

chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain n
    | even n = n:chain (n `div` 2)
    | odd n  = n:chain (n*3 + 1)

-- chain 10
-- [10,5,16,8,4,2,1]

-- chain 1
-- [1]

-- chain 30
-- [30,15,46,23,70,35,106,53,160,80,40,20,10,5,16,8,4,2,1]

numLongChains :: Int
numLongChains = length (filter isLong (map chain [1..100]))
    where isLong xs = length xs > 15

-- ==============
-- Lambdas
-- ==============

-- The \ symbol is used to make a lambda (because it looks slightly like the greek lambda sign)
-- The parameters are then entered, seperated by spaces
-- Finally -> and then the function body
-- They are usually surrounded by ( ) because otherwise they extend all the way to the right

-- Can be used to simplify the function above:
numLongChains' :: Int
numLongChains = length (filter (\xs -> length xs > 15) (map chain [1..100]))

-- Could write map (+3) [1,6,3,2] as map (\x -> x + 3) [1,6,3,2]
-- But is needless to as the former is more readable

-- Lambdas can take any number of parameters:
-- zipWith (\a b -> (a * 30 + 3) / b) [5,4,3,2,1] [1,2,3,4,5]
-- [153.0,61.5,31.0,15.75,6.6]

-- You can also do pattern matching in lambdas
-- If a pattern match fails in a lambda, a runtime error occurs, so be careful!

-- map (\(a,b) -> a + b) [(1,2),(3,5),(6,3),(2,6),(2,5)]
-- [3,8,9,8,7]

-- Due to the way functions are curried by default, these two are equivalent:
-- addThree :: (Num a) => a -> a -> a -> a
-- addThree x y z = x + y + z
-- addThree :: (Num a) => a -> a -> a -> a
-- addThree = \x -> \y -> \z -> x + y + z

flip' :: (a -> b -> c) -> b -> a -> c
flip' f = \x y -> f y x

-- ==============
-- Fold
-- ==============

-- foldl and foldr eat up values from the left or right of a list
-- they take an accumulator and a current value
-- foldl takes accumulator then current value (\acc x -> ...)
-- foldr takes current value then accumulator (\x acc -> ...)

sum' :: (Num a) => [a] -> a
sum' xs = foldl (\acc x -> acc + x) 0 xs
-- sum' [3,5,2,1]
-- 11
-- http://s3.amazonaws.com/lyah/foldl.png

-- could also write as:

sum'' :: (Num a) => [a] -> a
sum'' = foldl (+) 0

-- In general you can rewrite a function like 'foo a = bar b a' as 'foo = bar b' because of currying

elem' :: (Eq a) => a -> [a] -> Bool
elem' y ys = foldl (\acc x -> if x == y then True else acc) False ys
-- Starting value/accumulator is a boolean - we're assuming it's not in the list and setting True if it is

-- The accumulator value can be of any type. It can be a number, boolean, or even a new list.
-- For example, setting the starting element as an empty list:
map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr (\x acc -> f x : acc) [] xs

-- map' (+3) [1,2,3]
-- [4,5,6]

-- Approaches the list from the right hand side.
-- Take the last element (3) and apply the function to it (+3), which ends up being 6.
-- Then, we prepend it to the accumulator, which is []. 6:[] is [6] and that's now the accumulator.
-- We apply (+3) to 2, that's 5, and we prepend (:) it to the accumulator, so the accumulator is now [5,6].
-- Finally, we apply (+3) to 1 and prepend that to the accumulator and so the end value is [4,5,6]

-- The same function for a left fold would be:
map'' :: (a -> b) -> [a] -> [b]
map'' f xs = foldl (\acc x -> acc ++ [f x]) [] xs
-- but the ++ function is much more expensive than :, so usually use right folds when building up new lists from a list

-- One big difference is that right folds work on infinite lists -- left folds don't!

-- foldl1 and foldr1 functions work like foldl and foldr, but you don't need to provide them with an explicit starting value
-- They assume the first (or last if foldl1) element of the list to be the starting value, then start the fold with the element next to it
-- The sum function can be implemented like:
sum''' :: (Num a) => [a] -> a
sum''' = foldl1 (+)

maximum' :: (Ord a) => [a] -> a
maximum' = foldr1 (\x acc -> if x > acc then x else acc)

-- ==============
-- Scan
-- ==============

-- scanl and scanr are like foldl and foldr, only they report all the accumulator states in the form of a list.
-- e.g.
-- foldl (+) 0 [3,5,2,1]
-- 11

-- scanl (+) 0 [3,5,2,1]
-- [0,3,8,10,11]
-- http://s3.amazonaws.com/lyah/foldl.png

-- scanr (+) 0 [3,5,2,1]
-- [11,8,3,1,0]

-- scanl1 (\acc x -> if x > acc then x else acc) [3,4,5,3,7,9,2,1]
-- [3,4,5,5,7,9,9,9]

-- scanl (flip (:)) [] [3,2,1]
-- [[],[3],[2,3],[1,2,3]]

-- When using a scanl, the final result will be in the last element of the resulting list.
-- When using a scanr, the final result will place the result in the head.

-- Scans are used to monitor the progression of a function taht can be implemented as a fold.

-- ==============
-- $ Function
-- ==============
-- Function application with a space is left associative [so f a b c is the same as ((fa) b) c) .]
-- Most of the time it's a convenience function so don't have to write so many parentheses.
-- When a $ is encountered, the expression on it's right is applied as the paramter to the function on it's left.

-- sum (map sqrt [1..130])
-- can be rewritten as:
-- sum $ map sqrt [1..130]

-- sqrt (3 + 4 + 9)
-- can be rewritten as:
-- sqrt $ 3 + 4 + 9

-- $ sort of the the equivalent of writing an open parenthese and a closing one on the far right of the expression
-- sum (filter (> 10) (map (*2) [2..10]))
-- can be rewritten as:
-- sum $ filter (> 10) $ map (*2) [2..10]

-- Using $ means the function application can be treated just like another function.
-- This means you can map function application over a list of functions
-- map ($ 3) [(4+), (10*), (^2), sqrt]
-- [7.0,30.0,9.0,1.7320508075688772]

-- ==============
-- . Function Composition
-- ==============
-- Function Composition can be used to make functions on the fly and pass them to other functions.
-- Can use lambdas, but many times function composition is clearer and more concise.

-- map (\x -> negate (abs x)) [5,-3,-6,7,-3,2,-19,24]
-- [-5,-3,-6,-7,-3,-2,-19,-24]

-- Can be rewritten as:

-- map (negate . abs) [5,-3,-6,7,-3,2,-19,24]
-- [-5,-3,-6,-7,-3,-2,-19,-24]

-- Function Composition is right-associative, so many functions can be composed at a time.
-- The expression f (g (z x)) is equivalent to (f . g . z) x.

-- map (\xs -> negate (sum (tail xs))) [[1..5],[3..6],[1..7]]
-- [-14,-15,-27]

-- Can be rewritten as:

-- map (negate . sum . tail) [[1..5],[3..6],[1..7]]
-- [-14,-15,-27]

-- Function Composition takes one parameter, so if a function has multiple parameters they usually have to be partially applied.
-- sum (replicate 5 (max 6.7 8.9))
-- Can be rewritten as:
-- (sum . replicate 5 . max 6.7) 8.9
-- Or as:
-- sum . replicate 5 . max 6.7 $ 8.9
-- 44.5

-- What goes on is:
-- A function is created that takes what max 6.7 takes and applies replicate 5 to it.
-- Then a function is created that takes the result of that does a sum on it.
-- Finally, that function is called with 8.9.

-- But normally you can read it as:
-- Apply 8.9 to max 6.7, then apply replicate 5 to that, then apply sum to that.
-- Returning 44.5

-- replicate 100 (product (map (*3) (zipWith max [1,2,3,4,5] [4,5,6,7,8])))
-- Can be rewritten as:
-- replicate 100 . product . map (*3) . zipWith max [1,2,3,4,5] $ [4,5,6,7,8]

-- Chances are if the expression ends with 3 parentheses, you can translate it into 3 function composition operators.


-- Another common use of function composition is 'point free style'.
-- This function can be rewritten using point free style:
sum'''' :: (Num a) => [a] -> a
sum'''' xs = foldl (+) 0 xs
-- can be rewritten as:
sum'''' :: (Num a) => [a] -> a
sum'''' = foldl (+) 0
-- Because of currying, we can omit the xs on both sides, because calling foldl (+) 0 creates a function that takes a list.
-- Writing the function this way is called 'point free style'.

-- fn x = ceiling (negate (tan (cos (max 50 x))))
-- Can be rewritten as:
-- fn = ceiling . negate . tan . cos . max 50

-- 3 different ways of writing the oddSquareSum function:

oddSquareSum :: Integer
oddSquareSum = sum (takeWhile (<10000) (filter odd (map (^2) [1..])))

oddSquareSum' :: Integer
oddSquareSum' = sum . takeWhile (<10000) . filter odd . map (^2) $ [1..]

oddSquareSum'' :: Integer
oddSquareSum'' =
    let oddSquares = filter odd $ map (^2) [1..]
        belowLimit = takeWhile (<10000) oddSquares
    in  sum belowLimit
