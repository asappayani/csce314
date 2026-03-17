{-Aryaan Sappayani
    1/22/2026
-}

{- Write the functions sumList
                       doubleSum
                       sumEvenNumbers
                       sumDoubleEvenNumbers
                       sumDoubleOddNumbers
                       myFactorial
Then, use ghci to test each function
When ready, upload your code and paste your 
output to the questions below. -}

sumList :: [Int] -> Int
sumList [] = 0
sumList (x:xs) = x + sumList xs -- takes head, then adds it to the sum of the rest of list, recursive.

doubleSum :: [Int] -> Int
doubleSum x = 2 * sumList x

sumEvenNumbers :: [Int] -> Int
sumEvenNumbers x = sumList(filter even x)

sumDoubleEvenNumbers :: [Int] -> Int
sumDoubleEvenNumbers x = doubleSum(filter even x)

sumDoubleOddNumbers :: [Int] -> Int
sumDoubleOddNumbers x = doubleSum(filter odd x)

myFactorial :: Int -> Int
myFactorial 1 = 1
myFactorial x = x * myFactorial(x - 1)

main :: IO ()
main = do
  let numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
  print (sumList numbers)               -- Output: 55
  print (doubleSum numbers)             -- Output: 110
  print (sumEvenNumbers numbers)        -- Output: 30
  print (sumDoubleEvenNumbers numbers)  -- Output: 60
  print (sumDoubleOddNumbers numbers)   -- Output: 50
  print (myFactorial 5)                 -- Output: 120