
module Ex2 where 

type Poligonal = [Ponto]

data Ponto = Cartesiano Double Double | Polar Double Double
    deriving (Show,Eq)

data Figura = Circulo Ponto Double | Retangulo Ponto Ponto | Triangulo Ponto Ponto Ponto 
            deriving (Show,Eq)

--                  Ficha 1 exercicio 6

posx :: Ponto -> Double
posx (Cartesiano x _) = x
posx (Polar dis ang) = dis * cos ang

posy :: Ponto -> Double
posy (Cartesiano _ y) = y
posy (Polar dis ang) = dis * sin ang

raio :: Ponto -> Double
raio (Polar dis ang) = dis
raio (Cartesiano x y) = sqrt aux
        where aux = x^2 + y^2

angulo :: Ponto -> Double
angulo (Polar dis ang) = ang
angulo (Cartesiano x y) = atan2 y x

dist :: Ponto -> Ponto -> Double
dist p1 p2 = sqrt (dX + dY) 
    where dX = (posx p2 - posx p1)^2 
          dY = (posy p2 - posy p1)^2

--                  Ficha 1 exercicio 7 

area :: Figura -> Double
area (Triangulo p1 p2 p3) = 
                    let a = dist p1 p2 
                        b = dist p2 p3 
                        c = dist p3 p1 
                        s = (a+b+c) / 2
                    in sqrt (s*(s-a)*(s-b)*(s-c))    
area (Circulo _ raio) = pi * raio^2
area (Retangulo pt1 pt2) = la * com
    where la  = abs(posx pt1 - posx pt2)
          com = abs(posy pt1 - posy pt2)  

--                  Ficha 3 exercicio 2
--a)
comprimento :: Poligonal -> Double 
comprimento [] = 0 
comprimento [_] = 0
comprimento (h:b:t) = dist h b + comprimento (b:t)

--b)
fechada :: Poligonal -> Bool 
fechada p = length p >= 3 && head p == last p

--c)                            
triangula :: Poligonal -> [Figura] 
triangula (pt1:pt2:pt3:ps) | pt1 == pt3 = []
                           | otherwise = Triangulo pt1 pt2 pt3 : triangula (pt1:pt3:ps)
triangula _ = []

--d)                                                    
areaPol :: Poligonal -> Double 
areaPol l = areaPol' (triangula l)

-- Função auxiliar de areaPol que calcula a área de cada triângulo e soma as áreas
areaPol' :: [Figura] -> Double 
areaPol' [] = 0 
areaPol' (h:t) = area h + areaPol' t  

--e)                                                    
mover :: Poligonal -> Ponto -> Poligonal 
mover pol pt = pt : pol 

--f)                                                   
zoom :: Double -> Poligonal -> Poligonal 
zoom z (h:t) = mover (zoomAux z h t) h 

-- Função auxiliar de zoom que determina cada ponto da linha poligonal, aplicando o zoom
zoomAux :: Double -> Ponto -> Poligonal -> Poligonal 
zoomAux _ _ [] = []
zoomAux z p (h:t) = Cartesiano ((x-xp) * z + xp) ((y-yp) * z + yp) : zoomAux z p t 
        where x = posx h 
              y = posy h 
              xp = posx p 
              yp = posy p 

