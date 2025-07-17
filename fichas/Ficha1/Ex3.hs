
module Ex3 where

type Hora = (Int,Int)

--a)
valida :: Hora -> Bool
valida (h,m) = h>=0 && h<24 && m>=0 && m<60

--b)
apos :: Hora -> Hora -> Bool
apos (h1,m1) (h2,m2) = h1>h2 || (h1==h2 && m1>m2) 

--c)
converteM :: Hora -> Int
converteM (h,m) = m + h*60

--d)
converteH :: Int -> Hora
converteH min = (div min 60, mod min 60)

--e)
diferenca :: Hora -> Hora -> Int
diferenca h1 h2 = abs (converteM h1 - converteM h2) 

--f)
adiciona :: Int -> Hora -> Hora
adiciona t h = converteH (converteM h + t)

