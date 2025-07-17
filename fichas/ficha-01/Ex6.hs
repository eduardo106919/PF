
module Ex6 where

data Ponto = Cartesiano Double Double | Polar Double Double
    deriving (Show,Eq)

--a)
posx :: Ponto -> Double
posx (Cartesiano x _) = x
posx (Polar dis ang) = dis * cos ang

--b)
posy :: Ponto -> Double
posy (Cartesiano _ y) = y
posy (Polar dis ang) = dis * sin ang

--c)
raio :: Ponto -> Double
raio (Polar dis ang) = dis
raio (Cartesiano x y) = sqrt aux
        where aux = x^2 + y^2

--d)
angulo :: Ponto -> Double
angulo (Polar dis ang) = ang
angulo (Cartesiano x y) = atan2 y x

--e)
dist :: Ponto -> Ponto -> Double
dist p1 p2 = sqrt (dX + dY) 
    where dX = (posx p2 - posx p1)^2 
          dY = (posy p2 - posy p1)^2

