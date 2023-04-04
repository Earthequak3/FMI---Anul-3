
sqrImpar :: [Integer] -> Integer
sqrImpar = sum . map (^2) . filter odd

verifTrue :: [Bool] -> Bool
verifTrue  = foldr (&&) True

checkAll :: (Int -> Bool) -> [Int] -> Bool
checkAll p xs = foldr (\x acc -> acc && p x) True xs

anyVerifies :: (Int -> Bool) -> [Int] -> Bool
anyVerifies p xs = foldr (\x acc -> acc || p x) False xs

map'' :: (a -> b) -> [a] -> [b]
map'' f = foldr ((:) . f) []

filter'' :: (a -> Bool) -> [a] -> [a]
filter'' p = foldr (\x -> if p x then (x:) else id) []


listToInt :: [Integer] -> Integer
listToInt = foldl tenx 0
            where tenx a b = a * 10 + b

rmChar :: Char -> String -> String
rmChar c  = filter (\x -> x /= c)

rmCharsRec :: String -> String -> String
rmCharsRec [] s = s
rmCharsRec (x:xs) s 
                    | x `elem` s = rmCharsRec xs $ rmChar x s
                    | otherwise = rmCharsRec xs s

rmCharsFold :: String -> String -> String
rmCharsFold x s = foldr rmChar s x
