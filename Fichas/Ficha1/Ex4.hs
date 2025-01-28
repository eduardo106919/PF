
module Ex4 where

data Hora = H Int Int deriving (Show,Eq)

--a)                            
valida :: Hora -> Bool
valida (H h m) = h>=0 && h<24 && m>=0 && m<60

--b)
apos :: Hora -> Hora -> Bool
apos (H h1 m1) (H h2 m2) = h1>h2 || (h1==h2 && m1>m2)

--c)
converteM :: Hora -> Int 
converteM (H h m) = h*60 + m

--d)
converteH :: Int -> Hora
converteH temp = H (div temp 60) (mod temp 60)

--e)
diferenca :: Hora -> Hora -> Int
diferenca h1 h2 = abs (converteM h1 - converteM h2) 

--f)
adiciona :: Int -> Hora -> Hora
adiciona t h = converteH (converteM h + t)

