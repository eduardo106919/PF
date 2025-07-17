
module Ex2 where 

import Data.List

type Polinomio = [Monomio] 
type Monomio = (Float,Int)

--a) 
selgrau :: Int -> Polinomio -> Polinomio 
selgrau n = filter (\ (_,g) -> g == n)

--b) 
conta :: Int -> Polinomio -> Int 
conta n l = length $ selgrau n l

--c)                            
grau :: Polinomio -> Int
grau l = maximum $ map snd l 

--ou 
grau' :: Polinomio -> Int 
grau' = foldl (\ x (a,b) -> max x b) 0

--d)
deriv :: Polinomio -> Polinomio  
deriv = map (\ (x,y) -> (x * fromIntegral y,y-1)) 

--e) 
calcula :: Float -> Polinomio -> Float 
calcula n = foldl (\ ac (x,y) -> ac + x * n^y) 0 

--f)
simp :: Polinomio -> Polinomio 
simp = filter (\ (_,y) -> y/=0)

--g)
mult :: Monomio -> Polinomio -> Polinomio 
mult (a,b) = map (\ (x,y) -> (x*a,y+b))  

--h) 
ordena :: Polinomio -> Polinomio 
ordena = sortOn snd 

--i)                                
normaliza :: Polinomio -> Polinomio 
normaliza [] = []
normaliza p@((c,g):t) = somaGrau g (selgrau g p) : normaliza filtro 
    where filtro = filter (\ (x,y) -> y /= g) t 

-- Função que recebe uma lista de monómios do mesmo grau e soma-os
somaGrau :: Int -> Polinomio -> Monomio 
somaGrau n = foldl (\ (x,y) (x',y') -> (x+x',n)) (0,n)

--j)                                           
soma :: Polinomio -> Polinomio -> Polinomio 
soma p1 p2 = normaliza (p1 ++ p2)

--k) 
produto :: Polinomio -> Polinomio -> Polinomio 
produto p1 p2 = normaliza $ concat $ map ( `mult` p2) p1

--l)                                            
equiv :: Polinomio -> Polinomio -> Bool 
equiv p p' = ordena (normaliza p) == ordena (normaliza p')

