-- Laziness and Infinite Structures — Demo Script
-- Run this file using: runhaskell Demo.hs
-- Make sure to have the System.Random package available.

import System.Random (mkStdGen, randoms)

-- 1. Infinite List: Naturals
naturals :: [Int]
naturals = [0..]
demoNaturals = take 10 naturals
-- Output: [0,1,2,3,4,5,6,7,8,9]

-- 2. Infinite List of Random Numbers
--todo
-- Output: First 10 random numbers, after skipping first 5

-- 3. Infinite Primes (Sieve of Eratosthenes)
primes :: [Int]
primes = sieve [2..] 
  where sieve (p:xs) = p : sieve [ x | x <- xs, x `mod` p /= 0] 

demoPrimes = take 10 (drop 20 primes)
-- Output: Primes from position 21 to 30

-- 4. Fibonacci Sequence (Self-referential definition)
fibs :: [Int]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

demoFibs = take 15 fibs
-- Output: First 15 Fibonacci numbers

-- 5. Triangular Numbers
triangulars :: [Int]
triangulars = scanl1 (+)  naturals

demoTriangulars = take 10 (drop 20 triangulars)
-- Output: Triangular numbers positions 21-30

-- 6. zipWith Examples

-- A. Add even and odd numbers
evens :: [Int]
evens = [0,2..]

odds :: [Int]
odds = [1,3..]

demoSumList = take 10 (zipWith (+) evens odds)
hundreds = [100, 200, 300, 400]
demoSumListH = zipWith (+) hundreds evens
-- Output: Pointwise sum of evens and odds

-- B. zipWith on different lengths
--todo
-- Output: Multiplies first 5 Fibonacci numbers with first 5 naturals

-- C. zipWith random and Fibonacci lists
--todo
-- Output: Adds first 10 random numbers (mod 100) to first 10 Fibonacci numbers

-- Main function to display all results
main :: IO ()
main = do
  putStrLn "Demo: Lazy Infinite Lists in Haskell"
  putStrLn "\n1. First 10 natural numbers:"
  print demoNaturals

  -- putStrLn "\n2. 10 random numbers after dropping 5:"
  -- print demoRandoms

  putStrLn "\n3. Primes from 21 to 30:"
  print demoPrimes

  putStrLn "\n4. First 15 Fibonacci numbers:"
  print demoFibs

  putStrLn "\n5. Triangular numbers positions 21-30:"
  print demoTriangulars

  putStrLn "\n6A. Pointwise sum of first 10 evens and odds:"
  print demoSumList

  putStrLn "\n6B. adding evens with 100"
  print demoSumListH

--   putStrLn "\n6C. Adding 10 random numbers (mod 100) to first 10 Fibonacci numbers:"
--   print demoZipRandomFibs
