double :: Int -> Int -- Function signature, takes in an int and returns an int
double n = n + n -- Function definition, adds the input number to itself

quadruple :: Int -> Int
quadruple n = double n + double n

square :: Int -> Int
square n = n * n

getc :: (Double, Double) -> Double
getc (a, b) = sqrt(a^2 + b*b)

inRange :: Int -> Int -> Int -> Bool
inRange min max num = num >= min && num <= max

sumList :: [Int] -> Int
sumList [] = 0
sumList (head:rest) = head + sumList(rest) -- takes head, then adds it to the sum of the rest of list, recursive.

main = do
    putStrLn "The double of 22 is:"
    print(double 22)
    putStr "The square of 55 is: "
    print(square 55)
