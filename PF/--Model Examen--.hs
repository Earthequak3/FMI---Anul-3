--Model Examen--


data Point = Pt [Int]
    deriving Show


data Arb = Empty | Node Int Arb Arb
        deriving Show

class ToFromArb a where
    toArb :: a -> Arb
    fromArb :: Arb -> a

instance ToFromArb Point where
    toArb (Pt []) = Empty
    toArb (Pt (x:xs)) = Node x (toArb(Pt (filter (<x) xs))) (toArb(Pt (filter (>= x) xs )))
    fromArb Empty = Pt []
    fromArb (Node x st dr) = let Pt s = fromArb st
                                 Pt d = fromArb dr
                                 in Pt (s ++ [x] ++ d)

getFromInterval :: Ord a => a -> a -> [a] -> [a]
--getFromInterval x y l = [a | a <-l, a >= x, a <= y]
getFromInterval a b l = do
    elem <- l
    if a <= elem && elem <= b 
        then return elem 
        else []