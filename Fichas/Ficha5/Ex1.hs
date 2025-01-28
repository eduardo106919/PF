
module Ex1 where 

--a)
any' :: (a -> Bool) -> [a] -> Bool
any' _ [] = False 
any' f (h:t) | f h = True 
             | otherwise = any' f t 

--b) 
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (h:t) (h':t') = f h h' : zipWith' f t t' 

--c) 
takeWhile' :: (a -> Bool) -> [a] -> [a] 
takeWhile' _ [] = []
takeWhile' f (h:t) | f h = h : takeWhile' f t 
                   | otherwise = [] 

--d) 
dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' _ [] = []
dropWhile' f (h:t) | f h = dropWhile' f t
                   | otherwise = h:t

--e) 
span' :: (a -> Bool) -> [a] -> ([a],[a])
span' _ [] = ([],[])
span' f (h:t) | f h = (h:as,bs) 
              | otherwise = ([],h:t) 
    where (as,bs) = span' f t 

--f) 
deleteBy' :: (a -> a -> Bool) -> a -> [a] -> [a] 
deleteBy' _ _ [] = []
deleteBy' f ele (h:t) | f ele h = t 
                      | otherwise = h : deleteBy' f ele t 

--g) 
sortOn' :: Ord b => (a -> b) -> [a] -> [a] 
sortOn' _ [] = []
sortOn' f (h:t) = insertOn' f h (sortOn' f t) 

-- Função que insere um elemento numa lista ordenada 
insertOn' :: Ord b => (a -> b) -> a -> [a] -> [a] 
insertOn' _ x [] = [x] 
insertOn' f x (h:t) | f x > f h = h : insertOn' f x t  
                    | otherwise = x : h : t 

