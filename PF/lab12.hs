import Data.Monoid


elem1 :: (Foldable t, Eq a) => a -> t a -> Bool
elem1 x = foldr (\y acc -> acc || x == y) False

null1 :: (Foldable t) => t a -> Bool
null1 = foldr (\_ _ -> False) True

length1 :: (Foldable t) => t a -> Int
length1 = foldr(\_ acc -> acc + 1) 0

toList1 :: (Foldable t) => t a -> [a]
toList1 = foldr (:) []

fold1 :: (Foldable t, Monoid m) => t m -> m
fold1 = foldMap mempty -- Hint: folosiți foldMap

data Constant a b = Constant b
instance Foldable (Constant a) where
    foldMap f (Constant a) = f a

data Two a b = Two a b
instance Foldable (Two a ) where
    foldMap f (Two _ b) =  f b 
data Three a b c = Three a b c
instance Foldable (Three a b) where
    foldMap f (Three _ _ c) = f c

data Three' a b = Three' a b b
instance Foldable (Three' a ) where
    foldMap f (Three' _ b c) = f b <> f c

data Four' a b = Four' a b b b
instance Foldable (Four' a) where
    foldMap f (Four' _ b c d) = f b <> f c <> f d
data GoatLord a = NoGoat | OneGoat a | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)
instance Foldable GoatLord  where
    foldMap _ NoGoat = mempty
    foldMap f (OneGoat x) = f x
    foldMap f (MoreGoats x y z ) = foldMap f x <> foldMap f y <> foldMap f z
