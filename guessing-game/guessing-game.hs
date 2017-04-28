import System.Random (getStdGen, randomRs)
import Control.Monad (when)

main :: IO ()
main = do
    putStrLn "Guess the number!"

    secretNumber <- getRandomNumber
    guessLogic secretNumber

getRandomNumber :: IO Int
getRandomNumber = do
    randomNumber <- getStdGen
    return $ head $ randomRs (1, 100) randomNumber

repeatGame :: Int -> IO ()
repeatGame secret = do
    putStrLn "Please enter a number."
    guessLogic secret

guessLogic :: Int -> IO ()
guessLogic secret = do
    putStrLn "Please input your guess."
    rawGuess <- getLine
    if not $ null rawGuess
    then do
        let guess = reads rawGuess
        case guess :: [(Int, String)] of
            [(n, _)] -> do
                putStrLn $ "You guessed: " ++ show n
                putStrLn $ checkNumber n secret
                when (n /= secret) $ guessLogic secret
            _ -> repeatGame secret
    else repeatGame secret

checkNumber :: Int -> Int -> String
checkNumber x y
    | x < y     = "You guessed too low"
    | x > y     = "You guessed too high"
    | x == y    = "You're correct!"
    | otherwise = error "Can't calculate difference"
