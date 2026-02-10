-- CSCE 314: Week 2 In-Class Assignment
-- Mini Data Toolkit (Starter File)
-- Focus: functions, lists, list comprehensions, tuples, simple data types

module Main where

-- A record is: (name, quiz1, quiz2, quiz3)
type Record = (String, Int, Int, Int)

records :: [Record]
records =
  [ ("Ava",  10,  9,  8)
  , ("Ben",   7, 10,  6)
  , ("Cara",  9,  9, 10)
  , ("Dan",   6,  7,  8)
  , ("Eli",  10, 10, 10)
  , ("Finn",  8,  6,  9)
  ]

----------------------------------------------------------------------
-- Part A: Basic functions
----------------------------------------------------------------------

names :: [Record] -> [String]
names recordList = [name | (name, _, _, _) <- recordList]

scores :: Record -> [Int]
scores (_, score1, score2, score3) = [score1, score2, score3]

averageScore :: Record -> Int
averageScore (_, score1, score2, score3) = (score1 + score2 + score3) `div` 3

----------------------------------------------------------------------
-- Part B: Filtering and simple data types
----------------------------------------------------------------------

aboveAverage :: Int -> [Record] -> [String]
aboveAverage score recordList = [name | (name, score1, score2, score3) <- recordList, averageScore (name, score1, score2, score3) >= score]

data Status = Pass | Fail deriving (Show, Eq)

status :: Int -> Record -> Status
status cutoff record
    | averageScore record >= cutoff = Pass
    | otherwise = Fail

report :: Int -> [Record] -> [(String, Status)]
report cutoff recordList = [(name, status cutoff (name, s1, s2, s3)) | (name, s1, s2, s3) <- recordList]

----------------------------------------------------------------------
-- Part C: Working with totals and selection
----------------------------------------------------------------------

totalScore :: Record -> Int
totalScore (_, s1, s2, s3) = s1 + s2 + s3

bestRecord :: [Record] -> Record
bestRecord = undefined

removeOnce :: Record -> [Record] -> [Record]
removeOnce = undefined

topStudents :: Int -> [Record] -> [String]
topStudents = undefined
----------------------------------------------------------------------
-- Main (uncomment lines as you complete functions)
----------------------------------------------------------------------

main :: IO ()
main = do
  putStrLn "Mini Data Toolkit"

  putStrLn "Names: "
  print (names records)

  putStrLn "Scores for Ava: "
  print (scores ("Ava", 10, 9, 8))

  putStrLn "Above average >= 9: "
  print (averageScore ("Ava", 10, 9, 8))

  putStrLn "Report cutoff 8: "
  print (aboveAverage 9 records)

  putStrLn "Totals: "
  print (report 8 records)

  -- print (topStudents 2 records)