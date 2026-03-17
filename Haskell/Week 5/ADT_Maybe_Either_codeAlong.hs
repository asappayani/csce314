-- Live Coding: ADTs (record + non-record), polymorphism, Maybe, Either
-- Load in GHCi:
--   :load Week_ADT_Poly_MaybeEither_CodeAlong.hs

module Main where

----------------------------------------------------------------------
-- 1) ADT with record syntax: Pet
----------------------------------------------------------------------

data Species = Dog | Cat | Bird | Lizard
  deriving (Show, Eq)

data Pet = Pet
  { petName     :: String
  , petSpecies  :: Species
  , petBirthday :: (Int, Int, Int)  -- (year, month, day)
  , petWeight   :: Double           -- pounds (or kg—pick one and be consistent)
  , petColor    :: String
  } deriving (Show, Eq)

rex :: Pet
rex = Pet
  { petName = "Rex"
  , petSpecies = Dog
  , petBirthday = (2020, 5, 14)
  , petWeight = 42.5
  , petColor = "black"
  }

miso :: Pet
miso = Pet "Miso" Cat (2022, 9, 2) 9.1 "orange"

-- Small “use” example: string summary  
petSummary :: Pet -> String
petSummary p =
  petName p ++ " is a " ++ show (petSpecies p) ++ " (" ++ petColor p ++ ")."

-- Another “use” example: record update
gainWeight :: Double -> Pet -> Pet
gainWeight delta p = p { petWeight = petWeight p + delta }

----------------------------------------------------------------------
-- 2) ADT without record syntax: exactly 5 temperatures
----------------------------------------------------------------------

data Temp5 = Temp5 Double Double Double Double Double
  deriving (Show, Eq)

weekTemps :: Temp5
weekTemps = Temp5 71.2 68.9 73.5 70.0 69.8

toList5 :: Temp5 -> [Double]
toList5 (Temp5 a b c d e) = [a,b,c,d,e]

avgTemp5 :: Temp5 -> Double
avgTemp5 t =
  let xs = toList5 t
  in sum xs / fromIntegral (length xs)

maxTemp5 :: Temp5 -> Double
maxTemp5 (Temp5 a b c d e) = maximum [a,b,c,d,e]

----------------------------------------------------------------------
-- 3) Polymorphism examples
----------------------------------------------------------------------

-- Polymorphic length (works for any list)
myLength :: [a] -> Int
myLength []     = 0
myLength (_:xs) = 1 + myLength xs

-- Polymorphic safeHead (works for any list, returns Maybe)
safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (x:_) = Just x

-- Polymorphism + type class constraint (need Eq to compare)
count :: Eq a => a -> [a] -> Int
count _ [] = 0
count v (x:xs)
  | v == x    = 1 + count v xs
  | otherwise = count v xs

-- Multiple constraints: need Eq (==) and Show (printable)
describe2 :: (Eq a, Show a) => a -> a -> String
describe2 x y =
  if x == y
    then "They are equal: " ++ show x
    else "They are different: " ++ show x ++ " vs " ++ show y

----------------------------------------------------------------------
-- 4) Maybe and Either: safe failure instead of crashing
----------------------------------------------------------------------

safeDiv :: Int -> Int -> Maybe Int
safeDiv _ 0 = Nothing
safeDiv x y = Just (x `div` y)

safeDivMsg :: Int -> Int -> Either String Int
safeDivMsg _ 0 = Left "Division by zero!"
safeDivMsg x y = Right (x `div` y)

----------------------------------------------------------------------
-- main 
----------------------------------------------------------------------

main :: IO ()
main = do
  putStrLn "Live Coding: ADTs + Polymorphism + Maybe/Either"

  -- Pet ADT (record syntax)
  print rex
  print (petName rex)
  putStrLn (petSummary rex)
  print (gainWeight 1.2 rex)

  -- Temp5 ADT (no record syntax)
  print weekTemps
  print (toList5 weekTemps)
  print (avgTemp5 weekTemps)
  print (maxTemp5 weekTemps)

  -- Polymorphism
  print (myLength [1,2,3,4,5])
  print (myLength "howdy")                -- String is [Char]
  print (safeHead [10,20,30])
  print (safeHead ([] :: [Int]))

  -- Type class constraints
  print (count 3 [1,2,3,3,3,4])
  print (count 'a' "bananas")
  print (describe2 True False)
  print (describe2 rex rex)
  print (describe2 rex miso)

  -- Maybe / Either
  print (safeDiv 10 2)
  print (safeDiv 10 0)
  print (safeDivMsg 10 2)
  print (safeDivMsg 10 0)
