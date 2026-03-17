import Data.Char (digitToInt)

mult7 :: [Int]
mult7 = [7,14..98]

div3or5 :: [Int]
div3or5 = [x | x <- [1..50], x `mod` 3 == 0 || x `mod` 5 == 0]

squareAll :: [Int] -> [Int]
squareAll xs = map (\x -> x * x) xs

keepEven10to90 :: [Int] -> [Int]
keepEven10to90 xs = [x | x <- xs, even x, x >= 10, x <= 90]

sumDigits :: Int -> Int
sumDigits n = foldl (+) 0 (map digitToInt (show n))

addLists :: [Int] -> [Int] -> [Int]
addLists xs ys = zipWith (+) xs ys

allPairs :: [a] -> [b] -> [(a,b)]
allPairs xs ys = [(x, y) | x <- xs, y <- ys]

data Student = Student {
    sid :: Int,
    name :: String,
    gpa :: Double
}

isHonors :: Student -> Bool
isHonors s
    | gpa s >= 3.5 = True
    | otherwise = False

data Shape = 
    Circle Double
    | Rect Double Double
    deriving(Show, Eq)

area :: Shape -> Double
area (Circle r) = pi * (r * r)
area (Rect l w) = l * w

safeDiv :: Int -> Int -> Maybe Int
safeDiv _ 0 = Nothing
safeDiv x y = Just (x `div` y)

showDiv :: Int -> Int -> String
showDiv x y = 
    case safeDiv x y of
        Nothing -> "Divide by zero is not allowed"
        Just q -> show q

data Tree a = Empty | Node a (Tree a) (Tree a)
  deriving (Show, Eq)

balancedFromSorted :: [a] -> Tree a
balancedFromSorted [] = Empty
balancedFromSorted xs =
    let mid = length xs `div` 2
        (lst, x:rst) = splitAt mid xs
    in Node x (balancedFromSorted lst) (balancedFromSorted rst)

t100 :: Tree Int
t100 = balancedFromSorted [1..100]

height :: Tree a -> Int
height Empty = 0
height (Node _ ls rs) = 1 + max (height ls) (height rs)

leaves :: Tree a -> Int
leaves Empty = 0
leaves (Node _ Empty Empty) = 1
leaves (Node _ ls rs) = leaves ls + leaves rs

sumEvens :: Tree Int -> Int
sumEvens Empty = 0
sumEvens (Node x ls rs)
    | even x = x + sumEvens ls + sumEvens rs
    | otherwise = sumEvens ls + sumEvens rs







