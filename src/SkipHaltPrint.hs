module SkipHaltPrint where

data Cmd = Skip | Halt | Print String deriving(Show)
type Program = [Cmd]
type Context = String -> String

run :: IO ()
run = mapM_ (putStr . (++" ") . interp . fizzbuzz) [1..100]

interp :: [Context] -> String
interp c = foldr (.) id c ""

step :: Cmd -> String -> String
step Skip x = x
step Halt _ = ""
step (Print x) y = x ++ y

fizzbuzz :: Int -> [Context]
fizzbuzz n = (base . test 3 "Fizz" . test 5 "Buzz") [step Skip]
  where
    test d str
      | n `mod` d == 0 = \p -> [step $ Print str] ++ p ++ [step Halt]
      | otherwise = id
    base = \p -> p ++ [step $ Print (show n)]


