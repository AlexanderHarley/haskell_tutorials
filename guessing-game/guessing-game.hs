import System.Random

main :: IO ()
main = do
    putStrLn "Guess the number!"
    putStrLn "Please input your guess."

    secretNumber <- getRandomNumber
    putStrLn $ "Your secret number is: " ++ show (head secretNumber)

    guess <- getLine
    putStrLn $ "You guessed: " ++ guess

getRandomNumber :: IO [Int]
getRandomNumber = do
    randomNumber <- getStdGen
    return $ take 1 (randomRs (1, 100) randomNumber)
