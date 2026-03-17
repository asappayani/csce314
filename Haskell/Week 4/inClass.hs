-- CSCE 314: Week 4 In-Class Assignment
-- Haskell List Toolkit (Starter)
-- Concepts: ranges, list comprehensions, recursion, HOFs, lambdas, currying

module Main where

----------------------------------------------------------------------
-- Part A: List generation (ranges + functions)
----------------------------------------------------------------------

-- rangeStep start step end
-- Examples:
--   rangeStep 1 2 9   == [1,3,5,7,9]
--   rangeStep 10 (-3) 1 == [10,7,4,1]
rangeStep :: Int -> Int -> Int -> [Int]
rangeStep start step end = [x | x <- [start, start+step..end]]

-- first n multiples of k
-- multiplesOf 5 6 == [5,10,15,20,25,30]
multiplesOf :: Int -> Int -> [Int]
multiplesOf k n = [x*k | x <- [1..n] ]

-- all powers of 2 up to (and including) limit
-- powersOfTwoUpTo 20 == [1,2,4,8,16]
powersOfTwoUpTo :: Int -> [Int]
powersOfTwoUpTo limit = takeWhile(<=limit) [2^x | x <- [1..]]

----------------------------------------------------------------------
-- Part B: List comprehensions (build + filter + tuples)
----------------------------------------------------------------------

-- Pythagorean triples up to maxC:
-- pythagoreanTriples 10 includes (3,4,5), (6,8,10), etc.
-- Use a <= b to avoid duplicates like (4,3,5).
pythagoreanTriples :: Int -> [(Int, Int, Int)]
pythagoreanTriples maxC = [(a, b, c) | c <- [1..maxC], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2]

-- divisors of n (positive divisors)
-- divisors 12 == [1,2,3,4,6,12]
divisors :: Int -> [Int]
divisors n = [x | x <- [1..n], n `mod` x == 0]

-- primes up to maxN using divisors
-- primeList 20 == [2,3,5,7,11,13,17,19]
primeList :: Int -> [Int]
primeList maxN = [x | x <- [1..maxN], length (divisors x) == 2]

----------------------------------------------------------------------
-- Part C: Recursion beyond sum/product
----------------------------------------------------------------------

-- Split a list into chunks of size k.
-- chunksOf 3 [1..10] == [[1,2,3],[4,5,6],[7,8,9],[10]]
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf k xs = take k xs : chunksOf k (drop k xs)
 
-- Run-length encode consecutive duplicates.
-- compressRuns "aaabbc" == [('a',3),('b',2),('c',1)]
compressRuns :: Eq a => [a] -> [(a, Int)]
compressRuns xs = undefined

-- Prefix sums (scan-style) using recursion.
-- scanSums [1,2,3,4] == [1,3,6,10]
scanSums :: [Int] -> [Int]
scanSums [] = []
scanSums (x:xs) = x : scanSums xs


----------------------------------------------------------------------
-- Part D: Higher-order functions, lambdas, partial application
----------------------------------------------------------------------

-- Produce a small summary using your functions and HOFs.
-- Return a tuple:
-- (first10Odds, primesUpTo50, chunkedExample, encodedExample, prefixSumsExample)
toolkitDemo :: ([Int], [Int], [[Int]], [(Char, Int)], [Int])
toolkitDemo = undefined
-- the output of toolkitDemo looks like:
-- 
--([1,3,5,7,9,11,13,15,17,19],[2,3,5,7,11,13,17,19,23,29,31,37,41,43,47],
--[[1,4,9,16],[25,36,49,64],[81,100,121,144]],
--[('a',3),('b',2),('c',1),('d',4),('a',2)],[6,13,21,30,40,51,63,76,90,105])
----------------------------------------------------------------------
-- Main: uncomment as you finish functions
----------------------------------------------------------------------

main :: IO ()
main = do
  putStrLn "Week 4 In-Class: List Toolkit"

  print (rangeStep 1 2 9)
  print (rangeStep 10 (-3) 1)

  print (multiplesOf 5 6)
  print (powersOfTwoUpTo 200)

  print (take 5 (pythagoreanTriples 30))
  print (divisors 28)
  print (primeList 50)

  print (chunksOf 3 [1..10])
  -- print (compressRuns "aaabbcddddaa")
  -- print (scanSums [1,2,3,4,5])

  -- print toolkitDemo