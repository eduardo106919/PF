
module Ex2 where                    

--a)Int
nRaizes :: Float -> Float -> Float -> Int 
nRaizes a b c | root == 0 = 1 
              | root > 0  = 2 
              | otherwise = 0 
    where root = b^2 - 4*a*c

--b)  
raizes :: Float -> Float -> Float -> [Float]
raizes a b c | numero == 0 = []
             | numero == 1 = [-b / (2*a)]
             | otherwise   = [(-b + root) / (2*a), 
                              (-b - root) / (2*a)]
    where numero = nRaizes a b c
          root   = sqrt (b^2 - 4*a*c)

