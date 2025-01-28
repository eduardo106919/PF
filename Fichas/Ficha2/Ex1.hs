
module Ex1 where 

--a)
funA :: [Double] -> Double
funA [] = 0
funA (y:ys) = y^2 + (funA ys) 

{-
funA [2,3,5,1] = 2^2 + funA [3,5,1] 
= 4 + 3^2 + funA [5,1] = 4 + 9 + 5^2 + funA [1] 
= 13 + 25 + 1^2 + funA [] = 38 + 1 + 0 = 39 
-}

--b) 
funB :: [Int] -> [Int]
funB [] = []
funB (h:t) = if (mod h 2)==0 
             then h : (funB t)
             else (funB t)

{-
funB [8,5,12] = 8 : funB [5,12] = 8 : (mod 5 2 /= 0) funB [12] 
= 8 : 12 : fun [] = 8 : 12 : [] = [8,12] 
-}

--c) 
funC :: [a] -> [a] 
funC (x:y:t) = funC t
funC [x] = [x]
funC [] = []

{-
funC [1,2,3,4,5] = funC [3,4,5] = func [5] = [5]
-}

--d) 
funD :: [a] -> [a] 
funD l = g [] l
g :: [a] -> [a] -> [a] 
g acc [] = acc
g acc (h:t) = g (h:acc) t

{-
funD "otrec" = g [] "otrec" = g ('o':[]) "trec" 
= g ('t':"o") "rec" = g ('r':"to") "ec" = g ('e':"rto") "c"
= g ('c':"erto") [] = "certo"
-}

