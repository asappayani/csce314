module Main where

----------------------------------------------------------------------
-- Part 1: Pet Spa (record syntax)
----------------------------------------------------------------------

-- Use (year, month, day)
type Date = (Int, Int, Int)

data Species
  = Dog | Cat | Sheep | Bird | Horse | Pig | Snake | Lizard | Owl
  deriving (Show, Eq)

data Pet = Pet
  { petName         :: String
  , petBirthdate    :: Date
  , petSpecies      :: Species
  , petWeight       :: Double
  , petColor        :: String
  , petFavoriteFood :: String
  , petSound        :: String
  } deriving (Show, Eq)

spaPets :: [Pet]
spaPets =
  [ -- Dogs (3)
    Pet "Rex"      (2020,5,14)  Dog    42.5  "black"  "beef"     "woof"
  , Pet "Bella"    (2018,3,2)   Dog    35.2  "brown"  "chicken"  "woof"
  , Pet "Max"      (2022,11,10) Dog    50.1  "golden" "salmon"   "woof"

    -- Cats (3)
  , Pet "Miso"     (2022,9,2)   Cat     9.1  "orange" "tuna"     "meow"
  , Pet "Shadow"   (2019,1,15)  Cat    11.3  "black"  "turkey"   "meow"
  , Pet "Luna"     (2021,7,7)   Cat     8.4  "gray"   "chicken"  "meow"

    -- Sheep (2)
  , Pet "Baa"      (2018,3,10)  Sheep  80.0  "white"  "grass"    "baa"
  , Pet "Cloud"    (2016,6,20)  Sheep  95.5  "white"  "hay"      "baa"

    -- Birds (2)
  , Pet "Sky"      (2021,4,12)  Bird    2.3  "blue"   "seeds"    "chirp"
  , Pet "Sunny"    (2020,8,3)   Bird    1.9  "yellow" "berries"  "tweet"

    -- Horses (2)
  , Pet "Thunder"  (2015,9,30)  Horse 900.0  "brown"  "oats"     "neigh"
  , Pet "Star"     (2017,5,5)   Horse 850.0  "white"  "hay"      "neigh"

    -- Pigs (2)
  , Pet "Porky"    (2019,2,18)  Pig   210.0  "pink"   "corn"     "oink"
  , Pet "Rosie"    (2018,12,1)  Pig   195.0  "pink"   "apples"   "oink"

    -- Snakes (2)
  , Pet "Slither"  (2021,10,22) Snake  12.5  "green"  "mice"     "hiss"
  , Pet "Viper"    (2019,7,14)  Snake  15.0  "brown"  "rats"     "hiss"

    -- Lizards (2)
  , Pet "Spike"    (2020,6,6)   Lizard  3.4  "green"  "crickets" "..."
  , Pet "Emerald"  (2017,11,9)  Lizard  4.1  "green"  "worms"    "..."

    -- Owls (2)
  , Pet "Hoot"     (2019,10,1)  Owl     4.2  "brown"  "mice"     "hoot"
  , Pet "Athena"   (2016,1,25)  Owl     5.0  "white"  "rats"     "hoot"
  ]

-- TODO 1: Describe a pet (one readable sentence)
describePet :: Pet -> String
describePet pet = petName pet ++ " is a " ++ show (petSpecies pet) ++ " (" ++ petColor pet ++ ")." 

-- Helper: compare dates (older date = earlier)
-- Return True if d1 is earlier than d2
earlier :: Date -> Date -> Bool
earlier (y1,m1,day1) (y2,m2,day2) =
  (y1, m1, day1) < (y2, m2, day2)

-- TODO 2: Oldest pet in a list (Nothing if empty)
oldestPet :: [Pet] -> Maybe Pet
oldestPet [] = Nothing
oldestPet (x:xs) = Just (foldl pickOldest x xs)
    where 
        pickOldest current x = 
            if petBirthdate x `earlier` petBirthdate current
                then x
                else current

-- TODO 3: Heaviest pet in a list (Nothing if empty)
heaviestPet :: [Pet] -> Maybe Pet
heaviestPet [] = Nothing
heaviestPet (x:xs) = Just (foldl pickHeaviest x xs)
    where
        pickHeaviest current x = 
            if petWeight x > petWeight current
                then x
                else current

-- TODO 4: Lightest pet in a list (Nothing if empty)
lightestPet :: [Pet] -> Maybe Pet
lightestPet [] = Nothing
lightestPet (x:xs) = Just (foldl pickLightest x xs)
    where 
        pickLightest current x = 
            if petWeight x < petWeight current
                then x
                else current

----------------------------------------------------------------------
-- Part 2: Gradebook (no record syntax)
----------------------------------------------------------------------

-- Student name stored as (last, first)
type Name = (String, String)

data StudentRecord
  = StudentRecord Name Int
      Int Int Int Int Int Int Int Int Int Int
  deriving (Show, Eq)

classRecords :: [StudentRecord]
classRecords =
  [ -- A Students (90+)
    StudentRecord ("Nguyen","Ava")       10001 95 92 94 96 93 91 97 98 90 94
  , StudentRecord ("Patel","Cara")       10002 100 98 99 97 96 95 94 93 92 91
  , StudentRecord ("Smith","Liam")       10003 90 91 92 93 94 95 96 97 98 99
  , StudentRecord ("Chen","Olivia")      10004 93 94 95 92 91 90 96 97 94 95

    -- B Students (80–89)
  , StudentRecord ("Garcia","Ben")       10005 85 82 88 86 84 83 87 89 81 80
  , StudentRecord ("Brown","Mia")        10006 88 87 86 85 84 83 82 81 89 88
  , StudentRecord ("Lopez","Noah")       10007 80 82 84 86 88 87 85 83 81 89
  , StudentRecord ("Davis","Emma")       10008 89 88 87 86 85 84 83 82 81 80

    -- C Students (70–79)
  , StudentRecord ("Wilson","Ethan")     10009 75 72 78 76 74 73 77 79 71 70
  , StudentRecord ("Martinez","Sophia")  10010 70 71 72 73 74 75 76 77 78 79
  , StudentRecord ("Anderson","Lucas")   10011 79 78 77 76 75 74 73 72 71 70
  , StudentRecord ("Thomas","Isabella")  10012 74 75 76 77 78 79 70 71 72 73

    -- D Students (60–69)
  , StudentRecord ("Taylor","Mason")     10013 65 62 68 66 64 63 67 69 61 60
  , StudentRecord ("Moore","Amelia")     10014 60 61 62 63 64 65 66 67 68 69
  , StudentRecord ("Jackson","Logan")    10015 69 68 67 66 65 64 63 62 61 60
  , StudentRecord ("White","Harper")     10016 66 67 68 69 60 61 62 63 64 65

    -- F Students (< 60)
  , StudentRecord ("Harris","Aiden")     10017 50 55 58 52 54 53 57 59 51 56
  , StudentRecord ("Clark","Ella")       10018 40 45 48 42 44 43 47 49 41 46
  , StudentRecord ("Lewis","James")      10019 30 35 38 32 34 33 37 39 31 36
  , StudentRecord ("Walker","Grace")     10020 20 25 28 22 24 23 27 29 21 26
  ]

-- TODO 5: Extract scores from a StudentRecord as a list of 10 Ints
scoresOf :: StudentRecord -> [Int]
scoresOf (StudentRecord _ _ s1 s2 s3 s4 s5 s6 s7 s8 s9 s10) = [s1, s2, s3, s4, s5, s6, s7, s8, s9, s10]

-- TODO 6: Highest score in the entire class (Nothing if no students)
classHigh :: [StudentRecord] -> Maybe Int
classHigh [] = Nothing
classHigh records =
    let allScores = concat (map scoresOf records)
    in Just (foldl max (head allScores) (tail allScores))

-- TODO 7: Lowest score in the entire class (Nothing if no students)
classLow :: [StudentRecord] -> Maybe Int
classLow [] = Nothing
classLow records =
    let allScores = concat (map scoresOf records)
    in Just (foldl min (head allScores) (tail allScores))

-- TODO 8: Class average across ALL scores in ALL records (Nothing if empty)
classAverage :: [StudentRecord] -> Maybe Double
classAverage [] = Nothing
classAverage records = 
    let allScores = concat (map scoresOf records)
        total = fromIntegral (sum allScores)
        count = fromIntegral (length allScores)
    in Just (total / count)

-- TODO 9: Student average
studentAverage :: StudentRecord -> Double
studentAverage record =
    let scores = scoresOf record
        total = fromIntegral (sum scores)
        count = fromIntegral (length scores)
    in total / count

-- TODO 10: Letter grade from an average
letterGrade :: Double -> Char
letterGrade average 
    | average >= 90 = 'A'
    | average >= 80 = 'B'
    | average >= 70 = 'C'
    | average >= 60 = 'D'
    | otherwise = 'F'


-- TODO 11: Describe a student with name, ID, average, letter grade
describeStudent :: StudentRecord -> String
describeStudent = undefined


----------------------------------------------------------------------
-- Main (uncomment lines as you finish functions)
----------------------------------------------------------------------

main :: IO ()
main = do
  putStrLn "ADTs In-Class"

  -- Part 1 checks
  putStrLn (describePet (head spaPets))
  print (oldestPet spaPets)
  print (heaviestPet spaPets)
  print (lightestPet spaPets)

  -- Part 2 checks
  print (classHigh classRecords)
  print (classLow classRecords)
  print (classAverage classRecords)
  -- putStrLn (describeStudent (head classRecords))