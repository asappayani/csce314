-- CSCE 314 - Week 4 Code Along
-- Recursion, Lists, List Comprehensions, Higher-Order Functions, Lambdas, Currying
--
-- This file is meant to be used during live coding and also as a study reference.
-- Load in GHCi:
--   :load Week4_CodeAlong.hs
-- Then try the suggested expressions in the comments.

module Main where

----------------------------------------------------------------------
-- 1) Recursion follows the shape of a list: [] or (x:xs)
----------------------------------------------------------------------

sumList :: [Int] -> Int
sumList []     = 0
sumList (x:xs) = x + sumList xs

productList :: [Int] -> Int
productList []     = 1
productList (x:xs) = x * productList xs

-- A classic pattern: transform every element (like map)
doubleAll :: [Int] -> [Int]
doubleAll []     = []
doubleAll (x:xs) = (2 * x) : doubleAll xs

-- Generalize: our own map
map' :: (a -> b) -> [a] -> [b]
map' _ []     = []
map' f (x:xs) = f x : map' f xs

-- Filter pattern: keep elements that satisfy a predicate
filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs)
  | p x       = x : filter' p xs
  | otherwise = filter' p xs

----------------------------------------------------------------------
-- 2) List generation and laziness
----------------------------------------------------------------------

nums :: [Int]
nums = [1..10]

odds :: [Int]
odds = [1,3..]        -- infinite odds

firstTenOdds :: [Int]
firstTenOdds = take 10 odds

hundreds :: [Int]
hundreds = [100,200..] -- infinite multiples of 100

----------------------------------------------------------------------
-- 3) List comprehensions (math set-builder style)
----------------------------------------------------------------------

doublesLC :: [Int]
doublesLC = [x * 2 | x <- [1..5]]

evensLC :: [Int]
evensLC = [x | x <- [1..20], even x]

pairsLC :: [(Int, Int)]
pairsLC = [(x,y) | x <- [1..3], y <- [1..2]]

oddDoublesLC :: [Int]
oddDoublesLC = [x * 2 | x <- [1..10], odd x]

----------------------------------------------------------------------
-- 4) Higher-order functions (map, filter, foldr)
----------------------------------------------------------------------

sumHOF :: [Int] -> Int
sumHOF xs = foldr (+) 0 xs -- 0 is base case, in parathensis is what you do with each element

productHOF :: [Int] -> Int
productHOF = foldr (*) 1

doubleAllHOF :: [Int] -> [Int]
doubleAllHOF = map (*2)

-- Fold builds a result by combining elements
-- Example: rebuild the same list using foldr
rebuild :: [a] -> [a]
rebuild = foldr (:) []

----------------------------------------------------------------------
-- 5) Lambdas (anonymous functions)
----------------------------------------------------------------------

squares :: [Int] -> [Int]
squares xs = map (\x -> x * x) xs -- map takes in a function, here we use a lambda function and pass that which will be applied to xs

multiplesOf3 :: [Int] -> [Int]
multiplesOf3 xs = filter (\x -> x `mod` 3 == 0) xs

----------------------------------------------------------------------
-- 6) Currying and partial application
----------------------------------------------------------------------

add :: Int -> Int -> Int -- you can give it one variable, and it'll wait for another variable. ex: a3 = add 3. then do a3 7 which will give you 10
add x y = x + y

add5 :: Int -> Int
add5 = add 5

greaterThan5 :: Int -> Bool
greaterThan5 = (>5:t )

----------------------------------------------------------------------
-- 7) Quick demo values to print in main
----------------------------------------------------------------------

demoList :: [Int]
demoList = [1..10]

----------------------------------------------------------------------
-- main (kept simple so the file compiles and runs)
----------------------------------------------------------------------

main :: IO ()
main = do
  putStrLn "CSCE 314 - Week 4 Code Along (quick checks)"
  putStrLn ""
  putStrLn "Recursion:"
  print (sumList demoList)          -- 55
  print (productList [1..5])        -- 120
  print (doubleAll [1..5])          -- [2,4,6,8,10]
  putStrLn ""
  putStrLn "Our own map' and filter':"
  print (map' length ["hi","hello","howdy"])  -- [2,5,5]
  print (filter' odd demoList)      -- [1,3,5,7,9]
  putStrLn ""
  putStrLn "Ranges and laziness:"
  print nums
  print (take 5 odds)
  print (take 4 hundreds)
  putStrLn ""
  putStrLn "List comprehensions:"
  print doublesLC
  print evensLC
  print pairsLC
  print oddDoublesLC
  putStrLn ""
  putStrLn "Higher-order functions:"
  print (sumHOF demoList)
  print (productHOF [1..5])
  print (doubleAllHOF [1..5])
  print (rebuild [1,2,3])
  putStrLn ""
  putStrLn "Lambdas:"
  print (squares [1..5])
  print (multiplesOf3 [1..20])
  putStrLn ""
  putStrLn "Currying / partial application:"
  print (map add5 [1..10])
  print (filter greaterThan5 [1..10])

{- --------------------------------------------------------------------
Suggested GHCi experiments (copy/paste):

:load Week4_CodeAlong.hs
sumList [1,2,3,4]
productList [1..5]

-- See the list structure:
-- [] and (x:xs) pattern matching

map' (*2) [1..5]
map' (\s -> s ++ "!") ["hi","howdy"]

filter' even [1..20]
filter' (\x -> x `mod` 3 == 0) [1..30]

take 10 [1,3..]
take 10 (map (^2) [1..])

[x*2 | x <- [1..10], odd x]
[(x,y) | x <- [1..3], y <- [1..2]]

foldr (+) 0 [1..5]
foldr (:) [] [1,2,3]

map (\x -> x*x) [1..5]
(\x y -> x + y) 3 4

:add
:t add
:t (>5)
filter (>5) [1..10]
--------------------------------------------------------------------- -}
