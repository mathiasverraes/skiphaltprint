module SkipHaltPrint where

data Cmd = Skip | Halt | Print String deriving(Show)
type Program = [Cmd]
type Context = String -> String

run :: IO ()
run = mapM_ (putStr . (++" ")  . fizzbuzz) [1..100]

fizzbuzz :: Int -> String
fizzbuzz n = (test 3 "Fizz" . test 5 "Buzz") id (show n)
  where
    test d str
      | n `mod` d == 0 = \p -> (str ++) . p . const ""
      | otherwise = id


