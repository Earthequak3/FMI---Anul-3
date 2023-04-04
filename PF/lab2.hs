eeny :: Integer -> String
eeny x = if mod x 2 == 0 
        then "eeny"
        else "meeny"

fizzbuzz :: Integer -> String
fizzbuzz x | (mod x 15 == 0) = "FizzBuzz"
           |(mod x 3 == 0) = "Fizz"
           |(mod x 5 == 0) = "Buzz"
           |otherwise = ""
     
fibonacciCazuri :: Integer -> Integer
fibonacciCazuri n
    | n < 2     = n
    | otherwise = fibonacciCazuri (n - 1) + fibonacciCazuri (n - 2)
fibonacciEcuational :: Integer -> Integer
fibonacciEcuational 0 = 0
fibonacciEcuational 1 = 1
fibonacciEcuational n =
    fibonacciEcuational (n - 1) + fibonacciEcuational (n - 2)
tribonacci :: Integer -> Integer
tribonacci 1 = 1
tribonacci 2 = 1
tribonacci 3 = 2
tribonacci n = tribonacci (n-1) + tribonacci (n-2) + tribonacci (n-3)


verifL :: [Int] -> Bool
verifL n | length (n) `mod` 2 == 0 = True
         | otherwise = False


takefinal :: [a] -> Int -> [a]
takefinal l n = if n > length(l)
                then l
                else drop (length l - n) l

remove :: [a] -> Int -> [a]

remove l n = take (n-1) l ++ drop (n) l
-- semiPareRec [0,2,1,7,8,56,17,18] == [0,1,4,28,9]
semiPareRec :: [Int] -> [Int]
semiPareRec [] = []
semiPareRec (h:t)
    | even h    = h `div` 2 : t'
    | otherwise = t'
    where t' = semiPareRec t



totalLen :: [String] -> Int
totalLen [] = 0
totalLen (x:xs) = if head x =='A' 
                    then length x + totalLen xs
                    else 0 + totalLen xs
    

myreplicate :: Int -> Int -> [Int]
myreplicate n v
    | n > 0 = v : l
    | otherwise = l
    where l = myreplicate (n-1) v

sumImp :: [Int] -> Int
sumImp [] = 0
sumImp (h:t) 
    | odd h = h +  sumImp t
    | otherwise =  sumImp t


    
