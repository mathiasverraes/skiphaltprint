module SkipHaltPrint where

data Cmd = Skip | Halt | Print String deriving(Show)
type Program = [Cmd]
type Context = String -> String

run :: IO ()
run = mapM_ (putStr . (++" ")  . fizzbuzz) [1..100]

fizzbuzz :: Int -> String
fizzbuzz n = ((3 >=> "Fizz") . (5 >=> "Buzz")) id (show n)
  where
     test d str f
      | n `mod` d == 0 = const(str ++ f "")
      | otherwise = f
     (>=>) = test

