
module Ex4 where 

type Polinomio = [Monomio]
type Monomio = (Float,Int)

--a) 
conta :: Int -> Polinomio -> Int 
conta _ [] = 0
conta n (x:xs) | snd x == n = 1 + conta n xs 
               | otherwise = conta n xs 

--b) 
grau :: Polinomio -> Int 
grau [] = 0
grau (h:t) | snd h > grau t = snd h
           | otherwise = grau t 

--c)
selgrau :: Int -> Polinomio -> Polinomio
selgrau _ [] =  []
selgrau n (x:xs) | snd x == n = x : selgrau n xs 
                 | otherwise = selgrau n xs 

--d)
deriv :: Polinomio -> Polinomio 
deriv [] = []                                 
deriv ((a,b):xs) = (a*fromIntegral b,b-1) : deriv xs             

--e)
calcula :: Float -> Polinomio -> Float 
calcula n [] = 0
calcula n ((a,b):xs) = a * n^b + calcula n xs 

--f)
simp :: Polinomio -> Polinomio 
simp [] = []
simp (x:xs) | snd x /= 0 = x : simp xs 
            | otherwise = simp xs 

--g)
mult :: Monomio -> Polinomio -> Polinomio 
mult _ [] = []
mult (a,b) ((c,g):xs) = (a*c,b+g) : mult (a,b) xs 

--h)                                            
normaliza :: Polinomio -> Polinomio 
normaliza [] = []
normaliza (h:t) = soma' h t : normaliza (retira h t)

-- Função que soma os monomios do mesmo grau de um polinomio 
soma' :: Monomio -> Polinomio -> Monomio 
soma' m [] = m 
soma' (c,g) ((a,b):t) | g == b = soma' (c+a,g) t 
                      | otherwise = soma' (c,g) t

-- Função que retira os monomios de um certo grau de um polinomio
retira :: Monomio -> Polinomio -> Polinomio 
retira _ [] =  []
retira (c,g) ((a,b):t) | g == b = retira (c,g) t 
                       | otherwise = (a,b) : retira (c,g) t 

--i)                                            
soma :: Polinomio -> Polinomio -> Polinomio
soma p1 p2 = normaliza (p1 ++ p2) 

--j)                                                        
produto :: Polinomio -> Polinomio -> Polinomio
produto [] _ = [] 
produto (h:t) p = normaliza (mult h p ++ produto t p)

--k)                                            
ordena :: Polinomio -> Polinomio 
ordena [] = [] 
ordena (h:t) = insere h (ordena t) 

-- Função que insere um monomio num polinomio já ordenado
insere :: Monomio -> Polinomio -> Polinomio 
insere x [] = [x]
insere (a,b) ((c,d):t) | b <= d = (a,b) : ((c,d):t)
                       | otherwise = (c,d) : insere (a,b) t
 
--l)                                            
equiv :: Polinomio -> Polinomio -> Bool 
equiv p1 p2 = ordena (normaliza p1) == ordena (normaliza p2)

