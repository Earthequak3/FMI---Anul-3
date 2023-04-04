import Data.List

data Punct = Pt [Int]

data Arb = Vid | F Int | N Arb Arb
          deriving Show

class ToFromArb a where
 	    toArb :: a -> Arb
	    fromArb :: Arb -> a
instance Show Punct where
show (Pt []) = "()"
--show (Pt (x:xs)) = "(" ++  Prelude.show x ++ "," ++ Main.show xs ++ ")"
show (Pt xs) = "(" ++ (intercalate "," (map Prelude.show xs)) ++ ")"
      
-- Pt [1,2,3]
-- (1, 2, 3)

-- Pt []
-- ()

instance ToFromArb Punct where
  toArb (Pt coords) = foldr N Vid (map (F . fromIntegral) coords)
  fromArb arb = Pt (reverse $ f arb) where
    f Vid = []
    f (F x) = [fromIntegral x]
    f (N left right) = (f left) ++ (f right)


-- toArb (Pt [1,2,3])
-- N (F 1) (N (F 2) (N (F 3) Vid))
-- fromArb $ N (F 1) (N (F 2) (N (F 3) Vid)) :: Punct
--  (1,2,3)
data Geo a = Square a | Rectangle a a | Circle a
    deriving Show

class GeoOps g where
  perimeter :: (Floating a) => g a -> a
  area :: (Floating a) =>  g a -> a

-- ghci> pi
-- 3.141592653589793

instance GeoOps Geo where
  perimeter (Circle a) = 2 * pi * a
  perimeter (Square a) = 4 * a 
  perimeter (Rectangle l w) = 2 * (l+w)

  area (Square a) = a * a
  area (Rectangle l w) = l * w
  area (Circle a) = pi * a * a

instance (Eq a, Floating a) => Eq (Geo a) where
    (Square x1) == (Square x2) = perimeter (Square x1) == perimeter (Square y2)
    (Rectangle x1 y1) == (Rectangle x2 y2) = perimeter (Rectangle x1 y1) == perimeter (Rectangle x2 y2)
    (Circle x1) == (Circle x2) = perimeter (Circle x1) == perimeter (Circle x2)
    _ == _ = False
  


