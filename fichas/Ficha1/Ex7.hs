
module Ex7 where

import Ex6 

data Figura = Circulo Ponto Double
            | Retangulo Ponto Ponto
            | Triangulo Ponto Ponto Ponto
                deriving (Show,Eq)

--a)
poligono :: Figura -> Bool
poligono (Circulo _ _) = False
poligono (Retangulo p p') = posx p /= posx p' && posy p /= posy p' 
poligono (Triangulo pt1 pt2 pt3) = x1 /= x2 && x1 /= x3 && x2 /= x3 
                                && y1 /= y2 && y1 /= y3 && y2 /= y3
    where x1 = posx pt1 
          y1 = posy pt1 
          x2 = posx pt2 
          y2 = posy pt2 
          x3 = posx pt3 
          y3 = posy pt3 

--b)
vertices :: Figura -> [Ponto]
vertices (Retangulo p1 p2) = [p1, p2, Cartesiano x2 y1, Cartesiano x1 y2]
    where x1 = posx p1
          x2 = posx p2
          y1 = posy p1
          y2 = posy p2
vertices (Triangulo pt1 pt2 pt3) = [pt1, pt2, pt3]
vertices _ = []

--c)
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

--d)
perimetro :: Figura -> Double
perimetro (Circulo _ r) = 2 * pi * r
perimetro (Triangulo p1 p2 p3) = l1 + l2 + l3 
    where l1 = dist p1 p2 
          l2 = dist p2 p3
          l3 = dist p3 p1 
perimetro (Retangulo pt1 pt2) = 2*l + 2*c 
    where l = abs(posx pt1 - posx pt2)
          c = abs(posy pt1 - posy pt2)

