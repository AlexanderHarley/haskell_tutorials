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
main = do
    putStrLn "Hello, what's your name?"
    name <- getLine
    putStrLn ("Hey " ++ name ++ ", you rock!")

