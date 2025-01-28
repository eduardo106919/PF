
module Ex3 where 

import Ex1 

type Aluno = (Numero,Nome,Regime,Classificacao)

type Numero = Int
type Nome = String 

data Regime = ORD | TE | MEL deriving (Eq,Show)

data Classificacao = Aprov Int 
                   | Rep
                   | Faltou 
        deriving (Eq,Show)

type Turma = BTree Aluno 

arv :: BTree (Numero,Nome,Regime,Classificacao)
arv = Node (12,"edu",ORD,Aprov 19)
           (Node (7,"goncalo",TE,Aprov 17)
                 (Node (5,"maria",ORD,Faltou)
                       (Node (3,"carlos",MEL,Aprov 12) Empty Empty)
                       Empty
                 )
                 (Node (10,"jessica",ORD,Rep)
                       (Node (8,"carla",MEL,Faltou) Empty Empty)
                       Empty
                 )
           )
           (Node (15,"manel",ORD,Aprov 18)
                 (Node (13,"sofia",TE,Rep) Empty Empty)
                 (Node (18,"jervasio",MEL,Faltou)
                       (Node (16,"salvador",ORD,Aprov 20) Empty Empty)
                       Empty
                 )
           )

--a)
inscNum :: Numero -> Turma -> Bool 
inscNum _ Empty = False 
inscNum n (Node (i,_,_,_) l r) | n == i = True
                               | n < i = inscNum n l 
                               | otherwise = inscNum n r 

--b)
inscNome :: Nome -> Turma -> Bool 
inscNome _ Empty = False 
inscNome n (Node (_,nome,_,_) l r) = n == nome || inscNome n l || inscNome n r 

--c) 
trabEst :: Turma -> [(Numero,Nome)]
trabEst Empty = []
trabEst (Node (nom,n,TE,_) l r) = trabEst l ++ [(nom,n)] ++ trabEst r 
trabEst (Node _ l r) = trabEst l ++ trabEst r  

--d) 
nota :: Numero -> Turma -> Maybe Classificacao 
nota _ Empty = Nothing 
nota n (Node (i,nome,regime,clas) l r) 
    | n == i = Just clas 
    | n < i = nota n l
    | otherwise = nota n r 

--e) 
percFaltas :: Turma -> Float 
percFaltas Empty = 0
percFaltas arv = fromIntegral(percFaltas' arv * 100) / fromIntegral(contaNodos arv)

-- Função que conta o número de alunos que faltou à avaliação 
percFaltas' :: Turma -> Int 
percFaltas' Empty = 0 
percFaltas' (Node (_,_,_,Faltou) l r) = 1 + percFaltas' l + percFaltas' r 
percFaltas' (Node _ l r) = percFaltas' l + percFaltas' r

--f) 
mediaAprov :: Turma -> Float 
mediaAprov Empty = 0
mediaAprov arv = tot / val 
    where (tot,val) = aprovados arv

-- Função que determina o total de pontuação que os alunos, aprovados, tiveram e o número de alunos aprovados 
aprovados :: Turma -> (Float,Float)
aprovados Empty = (0,0)
aprovados (Node (_,_,_,a) l r) = case a of 
    Aprov x -> (fromIntegral x + vl + vr, 1 + nl + nr) 
    _ -> (vl + vr, nl + nr)
    where (vl,nl) = aprovados l 
          (vr,nr) = aprovados r

--g) 
aprovAv :: Turma -> Float 
aprovAv Empty = 0 
aprovAv arv = apro / aval 
    where (apro,aval) = aux arv 

-- Função que determina o número de alunos aprovados e o número de alunos avaliados 
aux :: Turma -> (Float,Float)
aux Empty = (0,0)
aux (Node (_,_,_,a) l r) = case a of 
    Aprov _ -> (1 + pl + pr, 1 + al + ar) 
    Rep -> (pl + pr, 1 + al + ar)
    _ -> (pl + pr, al + ar)
    where (pl,al) = aux l 
          (pr,ar) = aux r 

