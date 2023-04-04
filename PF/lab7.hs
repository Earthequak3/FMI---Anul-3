

data Expr = Const Int -- integer constant
          | Expr :+: Expr -- addition
          | Expr :*: Expr -- multiplication
           deriving Eq

data Operation = Add | Mult deriving (Eq, Show)

data Tree = Lf Int -- leaf
          | Node Operation Tree Tree -- branch
           deriving (Eq, Show)


instance Prelude.Show Expr where
show (Const x) =  Prelude.show x
show (x :+: y) = "(" ++ Prelude.show x ++ " + " ++ Prelude.show y ++ ")"
show (x :*: y) = "(" ++ Prelude.show x ++ " * " ++ Prelude.show y ++ ")"
            

evalExp :: Expr -> Int
evalExp (Const x) = x
evalExp (e1 :+: e2) = evalExp e1 + evalExp e2
evalExp (e1 :*: e2) = evalExp e1 * evalExp e2


exp1 = ((Const 2 :*: Const 3) :+: (Const 0 :*: Const 5))
exp2 = (Const 2 :*: (Const 3 :+: Const 4))
exp3 = (Const 4 :+: (Const 3 :*: Const 3))
exp4 = (((Const 1 :*: Const 2) :*: (Const 3 :+: Const 1)) :*: Const 2)
test11 = evalExp exp1 == 6
test12 = evalExp exp2 == 14
test13 = evalExp exp3 == 13
test14 = evalExp exp4 == 16

evalArb :: Tree -> Int
evalArb (Lf x) = x
evalArb (Node Add x y) = evalArb x + evalArb y
evalArb (Node Mult x y) = evalArb x * evalArb y


arb1 = Node Add (Node Mult (Lf 2) (Lf 3)) (Node Mult (Lf 0)(Lf 5))
arb2 = Node Mult (Lf 2) (Node Add (Lf 3)(Lf 4))
arb3 = Node Add (Lf 4) (Node Mult (Lf 3)(Lf 3))
arb4 = Node Mult (Node Mult (Node Mult (Lf 1) (Lf 2)) (Node Add (Lf 3)(Lf 1))) (Lf 2)

test21 = evalArb arb1 == 6
test22 = evalArb arb2 == 14
test23 = evalArb arb3 == 13
test24 = evalArb arb4 == 16


expToArb :: Expr -> Tree
expToArb (Const x) = Lf x
expToArb (x :+: y) = Node Add (expToArb x) (expToArb y)
expToArb (x :*: y) = Node Mult (expToArb x) (expToArb y)

-- 2 --
class Collection c where
  empty :: c key value
  singleton :: key -> value -> c key value
  insert
      :: Ord key
      => key -> value -> c key value -> c key value
  clookup :: Ord key => key -> c key value -> Maybe value
  delete :: Ord key => key -> c key value -> c key value
  keys :: c key value -> [key]
  keys c = map fst (toList c)
  values :: c key value -> [value]
  values c = map snd (toList c)
  toList :: c key value -> [(key, value)] 
  fromList :: Ord key => [(key,value)] -> c key value
  fromList [] = empty
  fromList ((k,v):xs) = insert k v (fromList xs)

newtype PairList k v
  = PairList { getPairList :: [(k, v)] }

instance Collection PairList where
 empty = PairList []
 singleton k v = PairList [(k, v)]
 insert k v (PairList pairs) = PairList $ (k, v) : pairs
 clookup k (PairList pairs) = lookup k pairs
 delete k (PairList pairs) = PairList $ filter ((/= k) . fst) pairs
 keys (PairList pairs) = map fst pairs
 values (PairList pairs) = map snd pairs
 toList(PairList pairs) = pairs
 fromList = PairList 


data SearchTree key value
  = Empty
  | BNode
      (SearchTree key value) -- elemente cu cheia mai mica
      key                    -- cheia elementului
      (Maybe value)          -- valoarea elementului
      (SearchTree key value) -- elemente cu cheia mai mare

instance Collection SearchTree where
  empty = Empty
  singleton k v = BNode Empty k (Just v) Empty
  
  insert k v t = case t of
    Empty -> BNode Empty k (Just v) Empty
    BNode l nk nv r ->
      if k < nk
      then BNode (insert k v l) nk nv r
      else BNode l nk nv (insert k v r)

  clookup k t = case t of
    Empty -> Nothing
    BNode l nk nv r -> if k < nk
    
      then clookup k l
        else if k > nk
          then clookup k r
            else nv
  
  delete k t = case t of
    Empty -> Empty
    BNode l nk nv r
      | k < nk -> BNode (delete k l) nk nv r
      | k > nk -> BNode l nk nv (delete k r)
      | nv == Nothing -> merge l r
      | otherwise -> BNode l nk Nothing r
    where 
      merge Empty r = r
      merge l Empty = l
      merge (BNode l1 x1 _ r1)(BNode l2 x2 _ r2) = BNode l1 x1 Nothing (BNode l2 x2 Nothing (merge r1 r2))

  keys t = case t of
    Empty -> []
    BNode l k _ r -> keys l  ++ [k] ++ keys r

  values t = concatMap (\x -> [x]) $ values' t
    where values' Empty = []
          values' (BNode l _ (Just v) r) = [v] ++ values' l ++ values' r
          values' (BNode l _ Nothing r) = values' l ++ values' r

  fromList [] = Empty
  fromList ((k, v):xs) = insert k v (fromList xs)