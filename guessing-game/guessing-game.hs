import System.Random (getStdGen, randomRs)
import Control.Monad (when)

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
    guess <- readLn
    putStrLn $ "You guessed: " ++ show guess
    putStrLn $ checkNumber guess secret
    when (guess /= secret) $ guessLogic secret

checkNumber :: Int -> Int -> String
checkNumber x y
    | x < y     = "You guessed too low"
    | x > y     = "You guessed too high"
    | x == y    = "You're correct!"
    | otherwise = error "Can't calculate difference"
