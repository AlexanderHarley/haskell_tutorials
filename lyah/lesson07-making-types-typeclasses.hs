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
