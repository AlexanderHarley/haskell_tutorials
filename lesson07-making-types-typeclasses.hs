-- Note: Don't import this file

-- The data keyword can be used to define own types
-- The 'Bool' type in the standard library is defined as:
data Bool = False | True

-- Both the type name and value constructors must be capitalized

-- In a similar way, 'Int' is defined something like this:
data Int = -2147483648 | -2147483647 | ... | -1 | 0 | 1 | 2 | ... | 2147483647
-- It's not actually defined like above, the '...' is being used as a placeholder for all the numbers in-between.
