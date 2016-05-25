module SkipHaltPrint where

run :: IO ()
run = mapM_ (putStr . (++" ")  . fizzbuzz) [1..100]

fizzbuzz :: Integer -> String
fizzbuzz n = rules id (show n)
  where
     test :: Integer -> String -> (String -> String) -> (String -> String)
     test d str f
      | n `mod` d == 0 = const(str ++ f "")
      | otherwise = f
     (>=>) = test

     rules =
          (3 >=> "Fizz")
        . (5 >=> "Buzz")
        . (7 >=> "Woo")