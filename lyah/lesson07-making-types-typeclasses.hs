-- Note: Don't import this file

-- The data keyword can be used to define own types
-- The 'Bool' type in the standard library is defined as:
data Bool = False | True

-- Both the type name and value constructors must be capitalized

-- In a similar way, 'Int' is defined something like this:
data Int = -2147483648 | -2147483647 | ... | -1 | 0 | 1 | 2 | ... | 2147483647
-- It's not actually defined like above, the '...' is being used as a placeholder for all the numbers in-between.

data Shape = Circle Float Float Float | Rectangle Float Float Float Float

:t Circle
-- Circle :: Float -> Float -> Float -> Shape

surface :: Shape -> Float
surface (Circle _ _ r) = pi * r ^ 2
surface (Rectangle x1 y1 x2 y2) = (abs $ x2 - x1) * (abs $ y2 - y1)
-- In the same way we can't write a function with a type declaration of True -> Int,
-- we couldn't write a type declaration of Circle -> Float because Circle is no a type, Shape is.

surface $ Circle 10 20 10
-- 314.15927

surface $ Rectangle 0 0 100 100
-- 10000.0

-- If we try to use Circle 10 20 10 in the prompt, we'll get an error.
-- That's because Haskell doesn't know how to display our data yet as a string.
data Shape = Circle Float Float Float | Rectangle Float Float Float Float deriving (Show)
-- This makes the type part of the Show typeclass. So now we can do this:

Circle 10 20 10
-- Circle 10.0 20.0 10.0

-- Value constructors are functions so we can map them and partially apply them etc.
-- If we want a list of concentric circles with different radii, we can do this:

map (Circle 10 20) [4,5,6,6]
-- [Circle 10.0 20.0 4.0, Circle 10.0 20.0 5.0, Circle 10.0 20.0 6.0, Circle 10.0 20.0 6.0]

-- The data type can be improved with an intermediate data type that defines a point in 2D space:
data Point = Point Float Float deriving (Show)
data Shape = Circle Point Float | Rectangle Point Point deriving (Show)

-- It's common to use the same name as the type if there's only one value constructor (e.g. in Point above)

surface :: Shape -> Float
surface (Circle (_ r) = pi * r ^ 2
surface (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)

surface $ Rectangle (Point 0 0) (Point 100 100)
-- 10000.0

surface $ Circle (Point 0 0) 24
-- Returns an exception:
-- *** Exception: Non-exhaustive patterns in function surface.

-- I adjusted the case for Circle to be:
surface (Circle (Point _ _) r) = pi * r ^ 2
-- The function now works correctly. Is there a nicer way to write this case?

surface $ Circle (Point 0 0) 24
-- 1809.5574

-- ==============================
-- Exporting Data Types/Functions
-- ==============================
-- At the top of the file:
 module Shapes
    ( Point(..),
      Shape(..),
      surface
    ) where
    -- // body of file

-- By doing Shape(..) we exported all the value constructors for Shape.
-- When importing the Shapes module, we have access to Rectangle and Circle.
-- Shape(..) is the same as doing Shape(Rectangle, Circle).

-- For example:

-- module Tests ( Test(..) ) where
-- data Test = Circle Float Float deriving (Show)

-- Loading into GHCi gives this result:
-- :t Circle
-- Circle :: Float -> Float -> Test

-- ==============================
-- Record Syntax
-- ==============================

-- Creating a data type that describes a person:
-- First Name, Last Name, Age, Height, Phone Number and Favourite Ice-Cream Flavour
data Person = Person String String Int Float String String deriving (Show)
person1 = Person "Haskell" "Guru" 30 184.2 "07770070070" "Strawberry"

-- Create some functions to retrieve details of that person in a more readable format:
firstName :: Person -> String
firstName (Person x _ _ _ _ _) = x

lastName :: Person -> String
lastName (Person _ x _ _ _ _) = x

age :: Person -> Int
age (Person _ _ x _ _ _) = x

height :: Person -> Float
height (Person _ _ _ x _ _) = x

phoneNumber :: Person -> String
phoneNumber (Person _ _ _ _ x _) = x

flavour :: Person -> String
flavour (Person _ _ _ _ _ x) = x

firstName person1 ++ " " ++ lastName person1
-- "Haskell Guru"

flavour person1
-- "Strawberry"


-- The above works, but there is a better way to write it - With the record syntax.
data Person = Person {
    firstName :: String,
    lastName :: String,
    age :: Int,
    height :: Float,
    phoneNumber :: String,
    flavour :: String
} deriving (Show)

-- By using record syntax, Haskell automatically makes the functions for firstName, lastName etc.
-- This can be demonstrated by:
:t flavour
-- flavour :: Person -> String

-- Using record syntax you don't have to necessarily put the fields in the proper order, as long as they are all listed.
-- If you don't use record syntax, they have to be specified in order.

-- ===================================
-- Type parameters / Type constructors
-- ===================================
-- A value constructor can take some values parameters and then produce a new value.
-- A type constructor can take types as parameters to produce new types.

-- For example, Maybe is implemented like this:
data Maybe a = Nothing | Just a

-- The `a` here is the type parameter. Because there's a type parameter involved, we call Maybe a type constructor.
-- Depending on what's passed to the type constructor, this could end up producing a type of `Maybe Int`, `Maybe String` etc.

-- Passing the value `Just 'a'` has a type of `Maybe Char`.
-- Here's some example of using the Maybe type:

Just "Haha"
-- Just "Haha"

Just 84
-- Just 84

:t Just "Haha"
-- Just "Haha" :: Maybe [Char]

:t Just 84
-- Just 84 :: (Num t) => Maybe t

:t Nothing
-- Nothing :: Maybe a

Just 10 :: Maybe Double
-- Just 10.0

-- Type parameters are useful because different types can be made with them depending on what kind of types we want contained in our data type.
-- When doing `Just "Haha"`, the type inference engine figures it out to be type `Maybe [Char]`.
-- If `a` in `Just a` is a string, then the `a` in `Maybe a` must also be a string.

-- Note: The type of `Nothing` is `Maybe a` - it's type is polymorphic.

-- Here is an example of a data type without Type Constructors:
data Car = Car {
    company :: String,
    model :: String,
    year :: Int
} deriving (Show)

-- And the same example with Type Constructors:
data Car a b c = Car {
    company :: a,
    model :: b,
    year :: c
} deriving (Show)

-- While Type Parameters aren't really that useful in the example above, it can be useful in circumstances where we don't care about the data type's value constructors.
-- For example: `Maybe` represents an option of either having nothing or having one of something. The type doesn't matter in that context.

-- Another good example of where Type Parameters are useful is with `Map k v` from `Data.Map`.
-- The `k` is the type of the keys. The `v` is the type of the values.
-- Having maps parameterized enables us to have mappings from any type to any other type as long as the type of the key is part of the `Ord` typeclass.


-- ===================================
-- The Functor typeclass
-- ===================================

class Functor f where
    fmap :: (a -> b) -> f a -> f b

-- fmap is interesting because `f` is not a concrete type (a type that a value can hold, e.g. Int, Bool, or Maybe String).
-- In fmap, `f` is type constructor that takes 1 parameter.

-- `Maybe Int` is a concrete type.
-- `Maybe` is a type constructor that takes one type as a parameter.

-- fmap takes a function from one type to another, and a functor applied with one type, and returns a functor applied with another type.

-- This is easier to think about when you look at the type signature for `map`:
map :: (a -> b) -> [a] -> [b]

-- So `map` takes a function from one type to another, and a list of one type and returns a list of another type.
-- Essentially, map is just a fmap that works only on lists.
-- Here's how the list is an instance of the Functor typeclass:
instance Functor [] where
    fmap = map

-- [] is a type constructor that takes one type and can produce types such as [Int], [String], or even [[String]].
-- If we were to write [a], this would be a concrete type (of a list with any type inside it), and wouldn't work as f has to be a type constructor.

-- Since for lists, fmap is just map, we get the same results when using them on lists:
fmap (*2) [1..3]
-- [2,4,6]
map (*2) [1..3]
-- [2,4,6]

-- If map or fmap was used over an empty list, it would return an empty list, because it turns an empty list of type [a] to an empty list of type [b].

-- Here's how Maybe is a functor
instance Functor Maybe where
    fmap f (Just x) = Just (f x)
    fmap f Nothing = Nothing

-- Note how again this is written as `instance Functor Maybe where` instead of for example: `instance Functor (Maybe m) where`.
-- This is for the same reason as above, Functor wants a type constructor that takes one type and not a concrete type.
-- With `instance Functor Maybe where`, the type signature of fmap acts like `(a -> b) -> Maybe a -> Maybe b`.
-- But with `instance Functor (Maybe m) where`, the type signature of fmap acts like `(a -> b) -> Maybe m a -> Maybe m b`, which doesn't make sense because Maybe takes just one type parameter.

-- The fmap implementation is fairly simple:
-- If it's an empty value of Nothing, then just return Nothing.
-- If it's not an empty value, but rather a single value packed up in a Just, then we apply the function on the contents of Just.

fmap (++ " Ho!") (Just "Hi!")
-- Just "Hi! Ho!"
fmap (++ " Ho!") Nothing
-- Nothing
fmap (*2) (Just 200)
-- Just 400


-- Either takes two parameters (e.g. Either a b), but can be made a Functor by partially applying Either.
-- Here's how `Either a` is a functor in the standard libraries:
instance Functor (Either a) where
    fmap f (Right x) = Right (f x)
    fmap f (Left x) = Left x

-- `Either a` is a type constructor that takes one parameter, whereas `Either` takes two.
-- If type signature for fmap acts like `(b -> c) -> Either a b -> Either a c`
