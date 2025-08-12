
module Exame2223 where 

import Data.Maybe

type MSet a = [(a,Int)] 

type Posicao = (Int,Int)
data Movimento = Norte | Sul | Oeste | Este
data Caminho = C Posicao [Movimento]

data Prop = Var String | Not Prop | And Prop Prop | Or Prop Prop    deriving Show

c :: MSet Char 
c = [('b',2),('a',4),('c',1)]

c' :: MSet Char 
c' = [('f',5),('c',2),('a',1),('m',3)]

p :: Prop 
p = Not (Or (And (Not (Var "A")) (Var "B")) (Var "C"))

--1 
--a) 
converteMSet :: MSet a -> [a] 
converteMSet l = concat $ map (\ (x,n) -> replicate n x) l

--b)                    Esta definição não é muito eficiente pois efetua-se duas travessias da lista
removeMSet :: Eq a => a -> MSet a -> MSet a 
removeMSet x l = filter (\ (_,m) -> m >= 1) aux 
    where aux = map (\ (y,n) -> if x==y 
                                then (y,n-1) 
                                else (y,n)) l 

--c) 
uniaoMSet :: Eq a => MSet a -> MSet a -> MSet a 
uniaoMSet [] l = l 
uniaoMSet l [] = l 
uniaoMSet (h:t) l = uniaoMSet t (insereMSet h l)

-- Função que insere um par num MSet 
insereMSet :: Eq a => (a,Int) -> MSet a -> MSet a 
insereMSet e [] = [e]
insereMSet (x,n) ((y,m):t) | x == y = (y,n+m) : t 
                           | otherwise = (y,m) : insereMSet (x,n) t 

--2 
-- Função que determina a posicao final de um caminho 
posicao :: Caminho -> Posicao 
posicao (C p []) = p 
posicao (C (x,y) (h:t)) = case h of 
    Norte -> posicao (C (x,y+1) t)
    Sul -> posicao (C (x,y-1) t)
    Oeste -> posicao (C (x-1,y) t)
    Este -> posicao (C (x+1,y) t)
    
eqCaminho :: Caminho -> Caminho -> Bool 
eqCaminho x@(C p l) x'@(C p' l') 
    = p == p' && c == c' && pf == pf' 
    where (c,c') = (length l,length l')
          (pf,pf') = (posicao x, posicao x')

instance Eq Caminho where 
    (==) = eqCaminho 

--3
func :: [[Int]] -> [Int] 
func l = concat (filter (\ x -> sum x > 10) l)

func' :: [[Int]] -> [Int]
func' [] = []
func' (h:t) | sum h > 10 = h ++ func t 
            | otherwise = func t 

--4 
--a)            DÚVIDA -> teria de definir a função fromJust ????
eval :: [(String,Bool)] -> Prop -> Bool 
eval l (Not p) = not (eval l p) 
eval l (And p p') = (eval l p) && (eval l p')
eval l (Or p p') = (eval l p) || (eval l p')
eval l (Var s) = fromJust $ lookup s l 

--b) 
nnf :: Prop -> Prop 
nnf (Not p) = case p of
    Not e -> nnf e 
    Or l r -> And (nnf (Not l)) (nnf (Not r)) 
    And l r -> Or (nnf (Not l)) (nnf (Not r)) 
    Var s -> Not (Var s) 
nnf p = p

