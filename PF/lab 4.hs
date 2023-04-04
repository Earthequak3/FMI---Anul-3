factori :: Integral a => a -> [a]
factori n = [i | i<-[1..n], mod n i == 0]




prim :: Int -> Bool
prim n 
        | isEmpty $ factori n = True 
        | otherwise = False
        where isEmpty l | length l == 2 = True 
                        | otherwise = False

numerePrime :: Int -> [Int]
numerePrime n = [x | x <- [1..n] , prim x ]

myzip3 :: [Int] -> [Int] -> [Int] -> [(Int,Int,Int)]
myzip3 xs ys zs = [(x,y,z) | (x,y,z) <- zip3 xs ys zs]


firstE1 :: [(a,b)] -> [a]
firstE1  = map fst 

sumList :: [[Int]] -> [Int]
sumList = map sum

pre12 :: [Int] -> [Int]
pre12  = map (\x -> if even x then x `div` 2 else x*2) 


check :: (Foldable t, Eq a) => a -> [t a] -> [t a]
check a  = filter (a `elem`) 

sqrImpare :: [Integer] -> [Integer]
sqrImpare = map (^2) . filter odd 

sqrPozImpare :: [Int] -> [Int]
sqrPozImpare l = map (^2) . filter odd $ map snd (zip l [1..])
                
numaiVocale :: [[Char]] -> [[Char]]
numaiVocale  = map ( filter  ( `elem` "AEIOUaeiou"))
                
