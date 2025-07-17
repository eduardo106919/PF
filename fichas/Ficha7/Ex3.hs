
module Ex3 where

data LTree a = Tip a | Fork (LTree a) (LTree a)
    deriving Show 

arv :: LTree Int 
arv = Fork 
           (Fork (Tip 3) 
                 (Fork (Tip 2) (Tip 1))
           )
           (Fork (Fork 
                       (Fork (Tip 4) (Tip 2))
                       (Tip 5)
                 )
                 (Fork (Tip 1) (Tip 3))
           )

--a) 
ltSum :: Num a => LTree a -> a 
ltSum (Tip x) = x
ltSum (Fork l r) = ltSum l + ltSum r 

--b) 
listaLT :: LTree a -> [a]
listaLT (Tip x) = [x]
listaLT (Fork l r) = listaLT l ++ listaLT r 

--c)                                    
ltHeight :: LTree a -> Int 
ltHeight (Tip _) = 0
ltHeight (Fork l r) = 1 + max (ltHeight l) (ltHeight r) 

