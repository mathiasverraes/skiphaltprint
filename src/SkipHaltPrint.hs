module SkipHaltPrint where

data Cmd = Skip | Halt | Print String deriving(Show)
type Program = [Cmd]
type Context = Program -> Program

run :: IO ()
run = mapM_ (putStr . (++" ") . interp . fizzbuzz) [1..100]

interp :: Program -> String
interp [] = ""
interp (Skip:cmds) = interp cmds
interp (Halt:_) = ""
interp (Print s:cmds) = s ++ interp cmds

fizzbuzz :: Int -> Program
fizzbuzz n = (base . test 3 "Fizz" . test 5 "Buzz") [Skip]
  where
    test d str
      | n `mod` d == 0 = \p -> [Print str] ++ p ++ [Halt]
      | otherwise = id
    base = \p -> p ++ [Print (show n)]


