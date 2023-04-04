import Data.Char 

nrVocale :: [String] -> Int
nrVocale xs = sum [countVocals x | x<-xs , isPalindrome x] 
                    where
                        countVocals x = length [c |c <- x,  c `elem` "aeiouAEIOU"]
                        isPalindrome x = x == reverse x

addElem :: Int -> [Int] -> [Int]
addElem _ [] = []
addElem n (x:xs) 
            | even x = x : n : addElem n xs
            | otherwise = x : addElem n xs

divizori :: Int -> [Int]
divizori x = [d |d <- take x [1..], mod x d == 0]

listadiv :: [Int] -> [[Int]]
listadiv l = [divizori x | x <- l ]

inIntervalComp :: Int -> Int -> [Int] -> [Int]
inIntervalComp a b l = [x | x <- l, x>=a,x<=b]

inIntervalRec :: Int -> Int -> [Int] -> [Int]
inIntervalRec _ _ [] = []
inIntervalRec a b (x:xs) 
                        | x >= a, x<=b = x:inIntervalRec a b xs
                        | otherwise = inIntervalRec a b xs

pozitiveC :: [Int] -> Int
pozitiveC l = length[x | x <-l,x>0]

pozitiveR :: [Int] -> Int
pozitiveR [] = 0
pozitiveR (x:xs) 
            | x > 0 = 1 + pozitiveR xs
            | otherwise = pozitiveR xs

oddPositions :: [Int] -> [Int]
oddPositions ls = oddPositions' ls 0
    where 
        oddPositions' [] _ = []
        oddPositions' (x:xs) n
            | odd x = n : oddPositions' xs (n+1)
            | otherwise = oddPositions' xs (n+1)


pozitiiImpare :: [Int] -> [Int]
pozitiiImpare l = [y | (x,y) <- zip l [0..],odd x]

multDigitsRec :: String -> Int
multDigitsRec [] = 1
multDigitsRec (x:xs) 
                    | isDigit x = digitToInt x * multDigitsRec xs
                    | otherwise = multDigitsRec xs
multDigitsComp :: String -> Int
multDigitsComp s = product[digitToInt x | x<-s, isDigit x]

getFromInterval :: Int -> Int -> [Int] -> [Int]
getFromInterval a b l = [x | x <-l, x >=a,x<=b]
