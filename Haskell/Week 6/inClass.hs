module Main where

import Data.Char (ord, chr)

----------------------------------------------------------------------
-- Infinite lists
----------------------------------------------------------------------

naturals :: [Int]
naturals = [0..]

-- TODO 1: infinite Fibonacci list
-- fibs should be [0,1,1,2,3,5,8,...]
fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

-- infinite cycle of letters a..z
letters :: [Char]
letters = cycle ['a'..'z']

-- TODO 2: zip naturals with letters into infinite list of tuples
-- example: [(0,'a'),(1,'b'),(2,'c')...]
natLetters :: [(Int, Char)]
natLetters = zipWith tuplify naturals letters
    where tuplify x y = (x, y)

----------------------------------------------------------------------
-- Helpers for slicing and reading input
----------------------------------------------------------------------

-- TODO 2.1: slice the list by dropping the first `skip` elements 
-- and taking the next `keep` elements
slice :: Int -> Int -> [a] -> [a]
slice skip keep xs = take keep (drop skip xs)

readInt :: String -> Int
readInt s = read s :: Int

readInteger :: String -> Integer
readInteger s = read s :: Integer

----------------------------------------------------------------------
-- Task 4 helper: letters after a given letter
----------------------------------------------------------------------

-- TODO 3: lettersAfter 'w' == "xyz"
-- lettersAfter 'a' == "bcdefghijklmnopqrstuvwxyz"
-- lettersAfter 'z' == ""
lettersAfter :: Char -> String
lettersAfter c = drop 1 [c..'z']

----------------------------------------------------------------------
-- Main program
----------------------------------------------------------------------

main :: IO ()
main = do
  putStrLn "Infinite Lists Practice"

  -- 1) Naturals slice
  putStrLn "\n(1) Naturals slice"
  putStrLn "Enter how many to skip:"
  skip1Str <- getLine
  putStrLn "Enter how many to keep:"
  keep1Str <- getLine
  let skip1 = readInt skip1Str
      keep1 = readInt keep1Str
  print (slice skip1 keep1 naturals)

  -- 2) Fibonacci
  putStrLn "\n(2) Fibonacci"
  putStrLn "How many Fibonacci numbers should I show?"
  nStr <- getLine
  let n = readInt nStr
  print (take n fibs)

  -- 3) Zip naturals with letters
  putStrLn "\n(3) Zip naturals with letters"
  putStrLn "Enter how many to skip:"
  skip3Str <- getLine
  putStrLn "Enter how many to keep:"
  keep3Str <- getLine
  let skip3 = readInt skip3Str
      keep3 = readInt keep3Str
  print (slice skip3 keep3 natLetters)

  -- 4) Letters after input letter
  putStrLn "\n(4) Letters after a given letter"
  putStrLn "Enter a lowercase letter (a-z):"
  letterStr <- getLine
  let c = head letterStr
  putStrLn (lettersAfter c)