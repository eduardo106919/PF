
module Ex2 where 

data RTree a = R a [RTree a]
    deriving Show
arv :: RTree Int 
arv = R 3 [ 
            R 5 [ R 1 []],
            R 3 [],
            R 2 [
                  R 6 [R 5 []],
                  R 3 [],
                  R 1 []
                ],
            R 7 [ R 2 [R 1 []]]
          ]

--a) 
soma :: Num a => RTree a -> a
soma (R x []) = x 
soma (R x l) = x + sum  (map soma l)

--b)
altura :: RTree a -> Int 
altura (R _ []) = 1 
altura (R _ l) = 1 + maximum (map altura l)

--c) 
prune :: Int -> RTree a -> RTree a 
prune 0 (R x _) = R x []
prune val (R x l) = R x (map (prune (val-1)) l)

--d) 
mirror :: RTree a -> RTree a 
mirror (R x l) = R x (map mirror (reverse l))

--e)                            esquerda -> direita -> topo              
postorder :: RTree a -> [a]
postorder (R x []) = [x]
postorder (R x l) = concat (map postorder l) ++ [x]

--                              topo -> esquerda -> direita
preorder :: RTree a -> [a]
preorder (R x []) = [x]
preorder (R x l) = x : concat (map preorder l)

