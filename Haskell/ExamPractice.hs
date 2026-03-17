data Paperweight = Paperweight {
    weight :: Float,
    color :: String,
    material :: String
} deriving (Show, Eq)

data Melonweights = Melonweights String Int Int Int Int Int deriving (Show)

listCompTest :: [(Int, Int, Int)]
listCompTest = [(x,y,z) | x <-[1..10], y <- [1..10], z <- [1..10], x < y, y < z, x + y + z == 12]

list1 :: [Int]
list1 = [10,20..40]

list2 :: [Int]
list2 = [1,2,3]

resultList :: [Int]
resultList = zipWith (\x y -> (x - y) * (x - y)) list1 list2

checkMe :: Int -> Int -> Bool
checkMe x y = x < y

initialMe :: String -> String -> (Char, Char)
initialMe s1 s2 = (head s1, head s2)

tryMe :: (a -> b) -> Int -> Int -> Maybe Int
tryMe f 0 0 = Nothing