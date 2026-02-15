module HomeWork1 where

-- CSCE 314 • Homework 1: Geometry Studio (Starter Stub)
-- Name: Aryaan Sappayani
-- UIN: 935009763
-- I worked independently, but I may have discussed general ideas with:

-- You may import NOTHING.

-- ===== 1) Core Shape model (ADT) =====
-- TODO: You will implement functions over this ADT using pattern matching and guards.
data Shape
  = Circle Double           -- radius
  | Rectangle Double Double -- width height
  | Square Double           -- side
  deriving (Eq, Show)

-- ===== 2) A recursive list for Shapes (recursive ADT) =====
-- TODO: You'll write list-like functions over this custom list.
data ShapeList
  = SEmpty
  | SCons Shape ShapeList
  deriving (Eq, Show)

-- ===== 3) Required functions =====
-- PART A: Single-shape utilities (pattern matching + guards)

-- area: Circle r -> pi*r^2; Rectangle w h -> w*h; Square s -> s*s
area :: Shape -> Double
area (Circle r) = pi * r * r
area (Rectangle w h) = w * h
area (Square s) = s * s

-- perimeter: Circle r -> 2*pi*r; Rectangle w h -> 2*(w+h); Square s -> 4*s
perimeter :: Shape -> Double
perimeter (Circle r) = 2 * pi * r
perimeter (Rectangle w h) = 2 * (w + h)
perimeter (Square s) = 4 * s

-- scale k shape: for k>0 scale dimensions by k; otherwise return shape unchanged (use a guard).
scale :: Double -> Shape -> Shape
scale k (Circle r) 
    | k > 0 = Circle (k * r)
    | otherwise = Circle r
scale k (Rectangle w h) 
    | k > 0 = Rectangle (k*w) (k*h)
    | otherwise = Rectangle w h
scale k (Square s)
    | k > 0 = Square (k * s)
    | otherwise = Square s

-- Identify shape “kind” exactly as these strings: "Circle" | "Rectangle" | "Square"
kind :: Shape -> String
kind (Circle r) = "Circle"
kind (Rectangle w h) = "Rectangle"
kind (Square s) = "Square"

-- PART B: Working with lists of shapes (recursion + higher-order functions)

-- Sum of areas of all shapes in a list
totalArea :: [Shape] -> Double
totalArea [] = 0
totalArea (x:xs) = area x + totalArea xs

-- Sum of perimeters of all shapes in a list
totalPerimeter :: [Shape] -> Double
totalPerimeter [] = 0
totalPerimeter (x:xs) = perimeter x + totalPerimeter xs

-- Count how many of each kind are present; return (circles, rectangles, squares)
countByKind :: [Shape] -> (Int, Int, Int)
countByKind [] = (0,0,0)
countByKind xs = foldl step (0,0,0) (map kind xs)
    where 
        step (circle, rect, square) k
            | k == "Circle" = (circle + 1, rect, square)
            | k == "Rectangle" = (circle, rect + 1, square)
            | otherwise = (circle, rect, square + 1)


-- Largest area in the list; Nothing for []
-- If areas tie, keep the FIRST (i.e., only replace when strictly greater).
maxByArea :: [Shape] -> Maybe Shape
maxByArea [] = Nothing
maxByArea (x:xs) = Just (foldl maxArea x xs)
    where 
        maxArea x xs = 
            if area xs > area x 
                then xs
                else x

-- Scale all shapes by k using your `scale` function
scaleAll :: Double -> [Shape] -> [Shape]
scaleAll k xs = map (scale k) xs

-- Keep only shapes with area >= t
filterByMinArea :: Double -> [Shape] -> [Shape]
filterByMinArea t xs = filter (\s -> area s >= t) xs

-- PART C: The recursive ShapeList ADT (structural recursion over SEmpty/SCons)

-- Convert a Haskell list [Shape] to ShapeList
toShapeList :: [Shape] -> ShapeList
toShapeList [] = SEmpty
toShapeList (x:xs) = (SCons x (toShapeList xs))

-- Convert ShapeList back to a Haskell list
fromShapeList :: ShapeList -> [Shape]
fromShapeList SEmpty = []
fromShapeList (SCons x xs) = x : fromShapeList xs

-- Sum of areas over ShapeList
sumAreasSL :: ShapeList -> Double
sumAreasSL SEmpty = 0
sumAreasSL (SCons x xs) = area x + sumAreasSL xs

-- Length of ShapeList
lengthSL :: ShapeList -> Int
lengthSL SEmpty = 0
lengthSL (SCons x xs) = 1 + lengthSL xs

-- Map-like operation over ShapeList using a function on Shape
mapSL :: (Shape -> Shape) -> ShapeList -> ShapeList
mapSL f SEmpty = SEmpty
mapSL f (SCons x xs) = (SCons (f x) (mapSL f xs))

-- ===== 4) A small built-in studio “scene” for your transcript tests =====
-- You can use this for quick local checks in ghci.
scene :: [Shape]
scene =
  [ Circle 3
  , Rectangle 2 5
  , Square 4
  , Circle 1
  , Rectangle 3 3
  ]

{- ===========================================================
INSTRUCTIONS FOR SUBMISSION

1) Implement the functions above (replace `undefined` bodies).
2) Do NOT add imports. Keep everything pure (no printing in functions).
3) In ghci, run the tests below and paste your results into this block.

Suggested ghci tests to include:

-- A) Single-shape utilities
area (Circle 3)
perimeter (Rectangle 2 5)
kind (Square 4)
scale 2 (Rectangle 1 2)
scale (-1) (Circle 10)

-- B) Lists of shapes (using `scene`)
scene
totalArea scene
totalPerimeter scene
countByKind scene
maxByArea scene
scaleAll 0.5 scene
filterByMinArea 10 scene

-- C) ShapeList
let sl = toShapeList scene
sl
lengthSL sl
sumAreasSL sl
mapSL (scale 2) sl
fromShapeList (mapSL (scale 2) sl)

=========================================================== -}