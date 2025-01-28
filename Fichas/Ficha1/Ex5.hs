
module Ex5 where

data Semaforo = Verde | Amarelo | Vermelho deriving (Show)

--a)
next :: Semaforo -> Semaforo 
next Verde = Amarelo
next Amarelo = Vermelho
next _ = Verde

--b)
stop :: Semaforo -> Bool
stop Vermelho = True
stop _ = False

--c)
safe :: Semaforo -> Semaforo -> Bool
safe _ Vermelho = True 
safe Vermelho _ = True 
safe _ _ = False 

