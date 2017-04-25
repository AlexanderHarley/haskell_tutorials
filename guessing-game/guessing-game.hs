import System.Random

main :: IO ()
main = do
    putStrLn "Guess the number!"
    putStrLn "Please input your guess."

    secretNumber <- getRandomNumber
    putStrLn $ "Your secret number is: " ++ show (head secretNumber)

    guessResult <- guessLogic $ head secretNumber
    putStrLn guessResult

getRandomNumber :: IO [Int]
getRandomNumber = do
    randomNumber <- getStdGen
    return $ take 1 $ randomRs (1, 100) randomNumber

guessLogic :: Int -> IO [Char]
guessLogic secret = do
    guess <- getLine
    putStrLn $ "You guessed: " ++ guess
    return $ checkNumber (read guess :: Int) secret

checkNumber :: Int -> Int -> [Char]
checkNumber x y
    | x < y  = "You guessed too low"
    | x > y  = "You guessed too high"
    | x == y = "You're correct!"
