module SkipHaltPrint where

data Cmd = Skip | Halt | Print String deriving(Show)
type Program = [Cmd]
type Context = String -> String

run :: IO ()
run = mapM_ (putStr . (++" ") . interp . fizzbuzz) [1..100]

interp :: [Context] -> String
interp c = foldr (.) id c ""

fizzbuzz :: Int -> [Context]
fizzbuzz n = (base . test 3 "Fizz" . test 5 "Buzz") [id]
  where
    test d str
      | n `mod` d == 0 = \p -> [(str ++)] ++ p ++ [const ""]
      | otherwise = id
    base = \p -> p ++ [((show n)++)]


