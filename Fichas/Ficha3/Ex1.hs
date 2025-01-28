
module Ex1 where 

data Hora = H Int Int deriving Show

type Etapa = (Hora,Hora)
type Viagem = [Etapa]

--              Ficha 1 exercicio 4     
                     
valida :: Hora -> Bool
valida (H h m) = h>=0 && h<24 && m>=0 && m<60

apos :: Hora -> Hora -> Bool
apos (H h1 m1) (H h2 m2) = h1>h2 || (h1==h2 && m1>m2)

converteM :: Hora -> Int 
converteM (H h m) = h*60 + m

converteH :: Int -> Hora
converteH temp = H (div temp 60) (mod temp 60)

diferenca :: Hora -> Hora -> Int
diferenca h1 h2 = abs (converteM h1 - converteM h2) 

adiciona :: Int -> Hora -> Hora
adiciona t h = converteH (converteM h + t)

--              Ficha 3 exercicio 1

--a)
etapaValida :: Etapa -> Bool 
etapaValida (inicio,fim) = (valida inicio && valida fim) && fim `apos` inicio

--b)                                        
viagemValida :: Viagem -> Bool
viagemValida [] = True
viagemValida ((h1,h2):t) = etapaValida (h1,h2) && viagemValida t && 
    (case t of 
          [] -> True
          (h3,h4):t' -> h3 `apos` h2)

--c)
partCheg :: Viagem -> (Hora,Hora)
partCheg (h:t) = (fst h, snd (last t))

--d) 
tempoViag :: Viagem -> Int 
tempoViag [] = 0
tempoViag ((a,b):t) = diferenca b a + tempoViag t 

--e)
tempoEsp :: Viagem -> Int 
tempoEsp viag = diferenca part cheg - tempo
    where (part,cheg) = partCheg viag 
          tempo = tempoViag viag 

--f)
tempoTotal :: Viagem -> Int 
tempoTotal viag = diferenca part cheg  
    where (part,cheg) = partCheg viag 


