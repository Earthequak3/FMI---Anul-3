{-
class Functor f where
fmap : : ( a -> b ) -> f a -> f b
-}
{-# LANGUAGE DerivingStrategies #-}
newtype Identity a = Identity a
instance Functor Identity where
  fmap f (Identity x) = Identity (f x)

data Pair a = Pair a a
instance Show a => Show (Pair a) where
    show (Pair x y) = "(" ++ show x ++ " , " ++ show y ++ ")"
instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)

data Constant a b = Constant b
instance Functor (Constant a) where
  fmap f (Constant x) = Constant (f x)
data Two a b = Two a b
instance Functor (Two a) where
    fmap f (Two x y) = Two x (f y)
data Three a b c = Three a b c
instance Functor (Three a b) where
    fmap f (Three x y z) = Three x y (f z)
data Three' a b = Three' a b b
instance Functor (Three' a) where
    fmap f (Three' x y z) = Three' x (f y) (f z)

data Four a b c d = Four a b c d
instance Functor (Four a b c) where
    fmap f (Four a b c d) = Four a b c (f d)
data Four'' a b = Four'' a a a b
instance Functor (Four'' a) where
    fmap f (Four'' a b c d) = Four'' a b c (f d)
data Quant a b = Finance | Desk a | Bloor b
instance Functor (Quant a ) where
    fmap f Finance = Finance
    fmap _ (Desk x) = Desk x
    fmap f (Bloor x) = Bloor (f x)

data LiftItOut f a = LiftItOut (f a)
instance Functor f => Functor (LiftItOut f) where
        fmap g (LiftItOut x) = LiftItOut (fmap g x)

data Parappa f g a = DaWrappa (f a) (g a)
instance (Functor f,Functor g) => Functor (Parappa f g) where
    fmap h (DaWrappa x y) = DaWrappa (fmap h x) (fmap h y)

data IgnoreOne f g a b = IgnoringSomething (f a) (g b)
instance Functor g => Functor (IgnoreOne f g a) where
    fmap h (IgnoringSomething x y ) = IgnoringSomething x (fmap h y)

data Notorious g o a t = Notorious (g o) (g a) (g t)
instance Functor g => Functor (Notorious g o a) where
    fmap h (Notorious a b c) = Notorious  a b (fmap h c)

data GoatLord a = NoGoat | OneGoat a | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)
instance Functor GoatLord where
    fmap _ NoGoat = NoGoat
    fmap f (OneGoat a) = OneGoat (f a)
    fmap f (MoreGoats a b c) = MoreGoats (fmap f a) (fmap f b) (fmap f c)

data TalkToMe a = Halt | Print String a | Read (String -> a)
instance Functor TalkToMe where
    fmap _ Halt = Halt
    fmap f (Print s x) = Print s (f x)
    fmap f (Read g) = Read (fmap f g)
