lucky :: (Integral a) => a -> String
lucky 7 = "LUCKY NUMBER SEVEN!"
lucky x = "Sorry, you're out of luck, pal!"

sayMe :: (Integral a) => a -> String
sayMe 1 = "One!"
sayMe 2 = "Two!"
sayMe 3 = "Three!"
sayMe 4 = "Four!"
sayMe 5 = "Five!"
sayMe x = "Not between 1 and 5"

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- factorial 5
-- 5*4*3*2*1*1
-- 120

-- function that extracts components of triples
first :: (a ,b, c) -> a
first (x, _, _) = x

second :: (a, b, c) -> b
second (_, y, _) = y

third :: (a, b, c) -> c
third (_, _, z) = z

-- pattern matching in list comprehensions
xs = [(1,3), (4,3), (2,4), (5,3), (5,6), (3,1)]
patternMatching tl = [a+b | (a,b) <- tl]
patternMatching xs
-- [4,7,6,8,11,4]

head' :: [a] -> a
head' [] = error "Can't call head on an empty list"
head' (x:_) = x

head' [4,5,6]
-- 4
head' "Hello"
-- 'H'

tell :: (Show a) => [a] -> String
tell [] = "The list is empty"
tell (x:[]) = "The list has one element: " ++ show x
-- ^ could be written as tell [x] = ...
tell (x:y:[]) = "The list has two elements: " ++ show x ++ " and " ++ show y
-- ^ could be written as tell [x,y] = ...
tell (x:y:_) = "This list is long. The first two elements are: " ++ show x ++ " and " ++ show y
-- ^ can't rewrite because it mathces any list of length 2 or more

length' :: (Num b) -> [a] -> b
length' [] = 0
length' (_:xs) = 1 + length' xs

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

bmiTell :: (RealFloat a) => a -> String
bmiTell bmi
    | bmi <= 18.5 = "Underweight"
    | bmi <= 25.0 = "Standard weight"
    | bmi <= 30.0 = "Overweight"
    | otherwise   = "Very Overweight"

bmiTell' :: (RealFloat a) => a -> a -> String
bmiTell' weight height
    | weight / height ^ 2 <= 18.5 = "Underweight"
    | weight / height ^ 2 <= 25.0 = "Standard weight"
    | weight / height ^ 2 <= 30.0 = "Overweight"
    | otherwise                   = "Very Overweight"

bmiTell'' :: (RealFloat a) => a -> a -> String
bmiTell'' weight height
    | bmi <= 18.5 = "Underweight"
    | bmi <= 25.0 = "Standard weight"
    | bmi <= 30.0 = "Overweight"
    | otherwise   = "Very Overweight"
    where bmi = weight / height ^ 2

bmiTell''' :: (RealFloat a) => a -> a -> String
bmiTell''' weight height
    | bmi <= skinny = "Underweight"
    | bmi <= normal = "Standard weight"
    | bmi <= fat = "Overweight"
    | otherwise   = "Very Overweight"
    where bmi = weight / height ^ 2
          (skinny, normal, fat) = (18.5, 25.0, 30.0)

max' :: (Ord a) => a -> a -> a
max' a b
    | a > b     = a
    | otherwise = b

myCompare :: (Ord a) => a -> a -> Ordering
a `myCompare` b
    | a > b     = GT
    | a == b    = EQ
    | otherwise = LT

initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
    where (f:_) = firstname
          (l:_) = lastname

calcBmis :: (RealFloat a) => [(a, a)] -> [a]
calcBmis xs = [bmi w h | (w, h) <- xs]
    where bmi w h = w / h ^ 2

-- 'let/in' like 'where' but puts the bindings first instead of after
cylinder :: (RealFloat a) => a -> a -> a
cylinder r h =
    let sideArea = 2 * pi * r * h
        topArea  = pi * r ^ 2
    in  sideArea + 2 * topArea

-- 'let/in' bindings are also expressions, so can be used in expressions unlike 'where'
-- for example:
4 * (let a = 9 in a + 1) + 2
-- 42

-- can also introduce functions in a local scope:
[let square x = x * x in (square 5, square 3, square 2)]
-- ([25,9,4])

-- seperating variables inline can be seperated with semicolons
(let a = 100; b = 200; c = 300 in a*b*c, let foo = "Hey "; bar = "there!" in foo ++ bar)
-- (6000000,"Hey there!")

-- pattern matching with let
(let (a,b,c) = (1,2,3) in a+b+c) * 100
-- 600

-- list comprehension with let
calcBmis' :: (RealFloat a) => [(a, a)] -> [a]
calcBmis' xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2]

-- let bindings can't be used across guards, but where can

-- case expressions can be used pretty much anywhere
describeList :: [a] -> String
describeList xs = "The list is " ++ case xs of [] -> "empty."
                                               [x] -> "a singleton list."
                                               xs -> "a longer list."

-- useful for pattern matching in the middle of an expression
-- can also be defined like this:
describeList :: [a] -> String
describeList xs = "The list is " ++ what xs
    where what [] = "empty."
          what [x] = "a singleton list."
          what xs = "a longer list."
