

newtype WriterN a = Writer { runWriter :: (a,[String]) } 

instance  Monad WriterN where
  return x = Writer (x,[])
  (Writer (x, l)) >>= f = let (x', l') = runWriter (f x) in Writer (x', l ++ l')

instance  Applicative WriterN where
    pure = return
    mf <*> ma = do
        f <- mf
        a <- ma
        return (f a)       
              
instance  Functor WriterN where              
    fmap f ma = pure f <*> ma     
              
tell :: String -> WriterN () 
tell log = Writer ((),[log])
                
logIncrement :: Int  -> WriterN Int
logIncrement x = do
    tell ("increment :" ++ show x)
    return (x+1)
logIncrement2 :: Int -> WriterN Int
logIncrement2 x = do
      y <- logIncrement x
      logIncrement y
logIncrementN :: Int -> Int -> WriterN Int
logIncrementN x 0 = return x
logIncrementN x n = do
    y <- logIncrement x
    logIncrementN y (n-1)
    

--logIncrementN x n = foldr (\_ acc -> acc >>= logIncrement) (return x) [1..n]

data Person = Person { name :: String, age :: Int }

showPersonN :: Person -> String
showPersonN (Person name age) = "NAME: " ++ name
showPersonA :: Person -> String
showPersonA (Person name age) = "AGE: " ++ show age
showPerson :: Person -> String
showPerson (Person name age) = "(" ++ showPersonN (Person name age) ++ "," ++ showPersonA (Person name age) ++ ")"