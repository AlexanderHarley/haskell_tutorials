import System.Random
import Control.Monad

main :: IO ()
main = do
    putStrLn "Guess the number!"

    secretNumber <- getRandomNumber
    -- putStrLn $ "Your secret number is: " ++ show (head secretNumber)
    guessLogic secretNumber

getRandomNumber :: IO Int
getRandomNumber = do
    randomNumber <- getStdGen
    return $ head $ randomRs (1, 100) randomNumber

guessLogic :: Int -> IO ()
guessLogic secret = do
    putStrLn "Please input your guess."
    guess <- getLine
    putStrLn $ "You guessed: " ++ guess
    putStrLn $ checkNumber (read guess :: Int) secret
    when ((read guess :: Int) /= secret) $ guessLogic secret

checkNumber :: Int -> Int -> String
checkNumber x y
    | x < y     = "You guessed too low"
    | x > y     = "You guessed too high"
    | x == y    = "You're correct!"
    | otherwise = error "Can't calculate difference"
