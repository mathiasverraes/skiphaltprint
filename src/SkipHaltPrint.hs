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
fizzbuzz n = (base n . fizz n . buzz n) [Skip]
  where
    fizz n
      | n `mod` 3 == 0 = \p -> [Print "Fizz"] ++ p ++ [Halt]
      | otherwise = \p -> p ++ [Skip]
    buzz n
      | n `mod` 5 == 0 = \p -> [Print "Buzz"] ++ p ++ [Halt]
      | otherwise = \p -> p ++ [Skip]
    base n = \p -> p ++ [Print (show n)]


