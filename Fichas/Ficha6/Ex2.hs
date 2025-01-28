
module Ex2 where 

data BTree a = Empty
             | Node a (BTree a) (BTree a)
             deriving (Show,Eq,Ord)

arv = Node 6 
           (Node 3
                 (Node 1
                       Empty
                       (Node 2 Empty Empty)
                 )
                 (Node 5
                       (Node 4 Empty Empty)
                       Empty
                 )
           )
           (Node 9 
                 (Node 7
                       Empty
                       (Node 8 Empty Empty)
                 )
                 Empty
           ) 
           
--a)
minimo :: Ord a => BTree a -> a 
minimo (Node i l r) | l == Empty = i 
                    | otherwise = minimo l 

--b) 
semMinimo :: Ord a => BTree a -> BTree a 
semMinimo m@(Node i l r) | i == minimo m = r 
                         | otherwise = Node i (semMinimo l) r

--c)
minSmin :: Ord a => BTree a -> (a,BTree a)
minSmin (Node i l r) | l == Empty = (i,r) 
                     | otherwise = (l1,Node i l2 r)
                        where (l1,l2) = minSmin l
                
--d)                                               
remove :: Ord a => a -> BTree a -> BTree a 
remove n (Node x l r) 
    | n < x = Node x (remove n l) r 
    | n > x = Node x l (remove n r)
    | otherwise = case r of 
        Empty -> l 
        _ -> Node v l t 
    where (v,t) = minSmin r 

