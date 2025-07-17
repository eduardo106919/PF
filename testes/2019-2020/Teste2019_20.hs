
module Teste2019_20 where 

type ConjInt = [Intervalo]
type Intervalo = (Int,Int)

data Contacto = Casa Integer 
              | Trab Integer 
              | Tlm Integer 
              | Email String 
    deriving Show 

type Nome = String 
type Agenda = [(Nome,[Contacto])]

data RTree a = R a [RTree a] deriving (Show, Eq) 

a :: Agenda 
a = [("xxx",[Email "JJJ",Tlm 444,Casa 999]),
     ("vvv",[Casa 222,Trab 555,Email "ccc"]),
     ("ppp",[Trab 333,Email "kkk",Email "qqq"])]

r :: RTree Int 
r = R 1 [R 2 [], 
         R 3 [R 4 [R 5 [], 
                   R 6 []]],
         R 7 []]

--1
--a)
intersect' :: Eq a => [a] -> [a] -> [a]
intersect' [] _ = []
intersect' _ [] = []
intersect' (h:t) l | h `elem` l = h : intersect' t l 
                   | otherwise = intersect' t l 

--b)
tails' :: [a] -> [[a]]
tails' [] = [[]]
tails' l = l : tails' (tail l)

--2
--a)
elems :: ConjInt -> [Int]
elems c = concat $ map (\ (x,y) -> [x..y]) c

--b) 
geraConj :: [Int] -> ConjInt 
geraConj [] = []
geraConj l = p : geraConj (drop c l)
    where seq = geraConj' l 
          p = (head seq,last seq)
          c = length seq 

-- Função que seleciona os números consecutivos numa lista 
geraConj' :: [Int] -> [Int]
geraConj' [] = []
geraConj' [x] = [x]
geraConj' (h:s:t) | s == succ h = h : geraConj' (s:t) 
                  | otherwise = [h]

--3 
--a) 
acrescEmail :: Nome -> String -> Agenda -> Agenda 
acrescEmail n s [] = [(n,[Email s])] 
acrescEmail n s ((x,l):t) | n == x = (x,(Email s):l) : t 
                          | otherwise = (x,l) : acrescEmail n s t 

--b) 
verEmails :: Nome -> Agenda -> Maybe [String]
verEmails _ [] = Nothing 
verEmails n ((x,l):t) | n == x = Just (mostra l)
                      | otherwise = verEmails n t 

-- Função que recolhe os email de uma lista de contactos
mostra :: [Contacto] -> [String]
mostra [] = []
mostra (Email s : t) = s : mostra t 
mostra (_:t) = mostra t 

--c) 
consulta :: [Contacto] -> ([Integer],[String])
consulta [] = ([],[])
consulta (h:t) = case h of 
    Email s -> (ns,s:es)
    Trab n -> (n:ns,es)
    Tlm n -> (n:ns,es)
    Casa n -> (n:ns,es)
    where (ns,es) = consulta t 

--d) 
consultaIO :: Agenda -> IO ()
consultaIO ag = do 
    putStr "Insira o nome que deseja: "
    nome <- getLine 
    let res = lookup nome ag
    case res of 
        Nothing -> putStrLn "O nome indicado não existe na agenda."
        Just l -> print l 

--4
--a) 
paths :: RTree a -> [[a]]
paths (R x []) = [[x]]
paths (R x l) = map (x:) $ concat $ map paths l 

--b) 
unpaths :: Eq a => [[a]] -> RTree a  
unpaths ([x]:_) = R x [] 
unpaths l = R ini $ map unpaths (aux sec)
    where ini = head $ head l
          sec = map (drop 1) l 

-- Função que separa as listas que têm a cabeça igual
aux :: Eq a => [[a]] -> [[[a]]] 
aux [] = []
aux m = agrupa m : aux (drop c m)
    where c = length $ agrupa m 

-- Função que recolhe as listas consecutivas com a mesma cabeça
agrupa :: Eq a => [[a]] -> [[a]]
agrupa [] = []
agrupa [x] = [x]
agrupa (h:s:t) | head s == head h = h : agrupa (s:t)
               | otherwise = [h]
    
