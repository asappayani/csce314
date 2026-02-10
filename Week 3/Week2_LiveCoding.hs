module Week2_LiveCoding where

-- Week 2 Live Coding: Functional Foundations
-- Topics: tuples, pattern matching, guards, lists/ranges, map/filter/fold, list comprehensions, Maybe
-- Use in GHCi:
--   :l Week2_LiveCoding.hs
--   :r

-- ------------------------------------------------------------
-- 1) Tuples
-- ------------------------------------------------------------

type Point = (Int, Int)

moveRight :: Int -> Point -> Point
moveRight dx (x, y) = (x + dx, y)

manhattan :: Point -> Int
manhattan (x, y) = abs x + abs y

-- ------------------------------------------------------------
-- 2) Pattern matching with lists
-- ------------------------------------------------------------

isEmpty :: [a] -> Bool -- "a" means any type
isEmpty []    = True
isEmpty (_:_) = False

preview2 :: Show a => [a] -> String
preview2 []      = "empty list"
preview2 [x]     = "one element: " ++ show x
preview2 (x:y:_) = "first two: " ++ show x ++ " and " ++ show y

-- ------------------------------------------------------------
-- 3) Guards (essentially if statements)
-- ------------------------------------------------------------

tempLabel :: Int -> String
tempLabel t
  | t <= 32   = "freezing"
  | t <= 60   = "cool"
  | t <= 80   = "warm"
  | otherwise = "hot"

describeNumber :: Int -> String
describeNumber n
  | n == 0    = "zero"
  | n < 0     = "negative"
  | otherwise = "positive"

-- ------------------------------------------------------------
-- 4) Lists + ranges
-- ------------------------------------------------------------

oneToTen :: [Int]
oneToTen = [1..10] -- creates a list of int from 1 to 10

everyThird :: [Int]
everyThird = [0,3..30] -- list of ints adding 3 each time

downFrom :: Int -> [Int]
downFrom n = [n, n-1 .. 0] -- takes in an int, and creates a list from num to 0

upFrom :: Int -> [Int] 
upFrom n =  [1, 2 .. n] -- takes an int and creates list from 1 to the int

fromTo :: (Int,Int) -> [Int] 
fromTo (i, j) = [i..j]

-- ------------------------------------------------------------
-- 5) Transformations: map, filter, fold
-- ------------------------------------------------------------

labelScores :: [Int] -> [String] -- take a list of int and return a list of strings
labelScores xs = map (\n -> "Score: " ++ show n) xs -- 

keepBig :: Int -> [Int] -> [Int]
keepBig threshold xs = filter (> threshold) xs

productAll :: [Int] -> Int
productAll xs = foldr (*) 1 xs

commaList :: [String] -> String
commaList xs = foldr combine "" xs
  where
    combine x ""  = x
    combine x acc = x ++ ", " ++ acc

-- ------------------------------------------------------------
-- 6) List comprehensions
-- ------------------------------------------------------------

grid :: Int -> [(Int, Int)]
grid n = [(x,y) | x <- [-n..n], y <- [-n..n]] -- does y first then x, so like creating matrix with 2 for loop

diagonalPoints :: Int -> [(Int, Int)]
diagonalPoints n = [(x,y) | x <- [0..n], y <- [0..n], x == y]

squaresOfEvens :: Int -> [Int]
squaresOfEvens n = [x*x | x <- [1..n], even x]

-- ------------------------------------------------------------
-- 7) Maybe example
-- ------------------------------------------------------------

safeDiv :: Int -> Int -> Maybe Int -- this is like a try and except
safeDiv _ 0 = Nothing
safeDiv x y = Just (x `div` y) -- 

showDiv :: Int -> Int -> String
showDiv x y =
  case safeDiv x y of
    Nothing -> "cannot divide by zero"
    Just q  -> "quotient = " ++ show q
