module ClassicFizzBuzzWoo () where

run :: IO ()
run = mapM_ (putStrLn . fizzbuzz) [1..100]

fizzbuzz :: Int -> String
fizzbuzz x
    | x `mod` 105 == 0 = "FizzBuzzWoo"
    | x `mod`  35 == 0 = "BuzzWoo"
    | x `mod`  21 == 0 = "FizzWoo"
    | x `mod`  15 == 0 = "FizzBuzz"
    | x `mod`   3 == 0 = "Fizz"
    | x `mod`   5 == 0 = "Buzz"
    | x `mod`   7 == 0 = "Woo"
    | otherwise        = show x