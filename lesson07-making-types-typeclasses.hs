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
