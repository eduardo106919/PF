
module Ex4 where 

data BTree a = Empty | Node a (BTree a) (BTree a)
    deriving Show

data LTree a = Tip a | Fork (LTree a) (LTree a)
    deriving Show

data FTree a b = Leaf b | No a (FTree a b) (FTree a b)
    deriving Show 

arv :: FTree Int Char 
arv = No 3 
          (No 4 
               (Leaf 'g') 
               (Leaf 'b')
          )
          (No 1 
               (No 5 (Leaf 'd') (Leaf 'a')) 
               (Leaf 'k')
          )

--a)
splitFTree :: FTree a b -> (BTree a,LTree b)
splitFTree (Leaf x) = (Empty,Tip x)
splitFTree (No x l r) = (Node x bl br, Fork ll lr)
    where (bl,ll) = splitFTree l 
          (br,lr) = splitFTree r 

--b)                                                    
joinTrees :: BTree a -> LTree b -> Maybe (FTree a b)
joinTrees Empty (Tip x) = Just (Leaf x) 
joinTrees (Node x l r) (Fork l' r') = 
    case (joinTrees l l', joinTrees r r') of 
        (Just bt, Just lt) -> Just (No x bt lt)
        _ -> Nothing 
joinTrees _ _ = Nothing 

