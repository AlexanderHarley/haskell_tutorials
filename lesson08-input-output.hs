-- To compile a Haskell program using GHC you use the command:
-- ghc --make filename

-- To compile output to a different folder:
-- ghc --make -o build/sample.exe src/sample.hs

-- main = putStrLn "hello, world"

-- putStrLn has a type of:
-- putStrLn :: String -> IO ()

-- It takes a string and returns an I/O action
-- An I/O action is something that, when performed, will carry out an action with a side-effect,
-- and will contain some kind of return value inside it.

-- Printing a string to the terminal doesn't really have any kind of meaningful return value,
-- so a dummy value of () is used.

-- ========
-- Main
-- ========
-- An I/O action will be performed when we give it a name of main and then run the program.

-- For example:
-- main = do
--     putStrLn "Hello, what's your name?"
--     name <- getLine
--     putStrLn ("Hey " ++ name ++ ", welcome!")

-- By using the do syntax you can glue commands together into one I/O action.
-- The action produced as a type of IO (), because that's the tpye of the last I/O action inside.

-- The type of a sequence of commands is the type of the last line. Which means the above can be written as:
main = do
    putStrLn "Hello, what's your name?"
    getLine

-- getLine already has the type of a command that returns a string so it can just be made the last line.
-- Note: This isn't exactly the same. Will only print the name as opposed to the name w/ strings.

-- ========
-- name <- getLine
-- ========
-- This can be read as 'perform the I/O action getLine and then bind its result value to name'
-- getLine has a type of IO String, so name will have a type of String.

-- The I/O action can be thought of as a box with feet that goes out and maybe brings back some data.
-- Once it's fetched that data, the only way to open the box is the use the <- construct.

-- If we're taking data out of an I/O action, we can only take it out when we're inside another I/O action.
-- This is how Haskell manages to neatly seperate the pure and impure parts of code.

-- getLine is in a sense impure because its result value is not guaranteed to be the same when performed twice.
-- When we do 'name <- getLine', name is just a normal String, and can be used as a normal String with other functions

