
module Ex1 where 

data BTree a = Empty 
             | Node a (BTree a) (BTree a)
        deriving Show

arv1 = Node 3 
           (Node 4
                 Empty
                 (Node 4
                       (Node 7 
                             Empty 
                             (Node 1 Empty Empty))
                       (Node 3 
                             (Node 6 Empty Empty) 
                             Empty)
                 )
           )
           (Node 5
                 (Node 9 
                       (Node 2 
                             Empty 
                             (Node 8 Empty Empty))
                       (Node 0 Empty Empty)
                 )
                 (Node 1 Empty Empty)
           )

arv2 = Node 6 Empty
              (Node 1 
                    (Node 9 
                          (Node 4 Empty Empty)
                          (Node 8 
                                Empty 
                                (Node 3 Empty Empty))
                    )
                    (Node 0 
                          (Node 2 
                                Empty
                                (Node 5 Empty Empty)
                          )
                          (Node 7 Empty Empty)))

--a) 
altura :: BTree a -> Int 
altura Empty = 0 
altura (Node _ l r) = 1 + max (altura l) (altura r)

--b)
contaNodos :: BTree a -> Int 
contaNodos Empty = 0
contaNodos (Node _ l r) = 1 + contaNodos l + contaNodos r

--c) 
folhas :: BTree a -> Int 
folhas Empty = 0 
folhas (Node _ Empty Empty) = 1 
folhas (Node _ l r) = folhas l + folhas r 

--d)                                   
prune :: Int -> BTree a -> BTree a 
prune 0 _ = Empty 
prune _ Empty = Empty 
prune n (Node i l r) = Node i (prune (n-1) l) (prune (n-1) r)

--e) 
path :: [Bool] -> BTree a -> [a]
path [] (Node x _ _) = [x]
path _ Empty = []
path (h:t) (Node i l r) | h = i : path t r 
                        | otherwise = i : path t l 

--f) 
mirror :: BTree a -> BTree a 
mirror Empty = Empty 
mirror (Node i l r) = Node i (mirror r) (mirror l)

--g)
zipWithBT :: (a -> b -> c) -> BTree a -> BTree b -> BTree c 
zipWithBT f (Node i l r) (Node i' l' r') = Node (f i i') (zipWithBT f l l') (zipWithBT f r r')
zipWithBT _ _ _ = Empty 

--h) 
unzipBT :: BTree (a,b,c) -> (BTree a,BTree b,BTree c) 
unzipBT (Node (a,b,c) l r) = (Node a x1 y1,Node b x2 y2,Node c x3 y3)
    where (x1,x2,x3) = unzipBT l 
          (y1,y2,y3) = unzipBT r
unzipBT Empty = (Empty,Empty,Empty)


