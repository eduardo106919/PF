
module Ex1 where 

import System.Random 
import Data.List

type FilePath = String 

--a)
bingo :: IO ()
bingo = do bingoAux [1..10]
           putStrLn "BINGO !!!"

bingoAux :: [Int] -> IO ()
bingoAux [] = return ()
bingoAux list = do 
    numSort <- randomRIO (1,90 :: Int)
    if numSort `elem` list 
    then do putStr "Prima ENTER para sortear um número: "
            getChar 
            print numSort 
            bingoAux (delete numSort list)
    else bingoAux list
                    
--b) 
mastermind :: IO ()
mastermind = do 
    n1 <- randomRIO (0,9 :: Int) 
    n2 <- randomRIO (0,9 :: Int) 
    n3 <- randomRIO (0,9 :: Int) 
    n4 <- randomRIO (0,9 :: Int) 
    let seq = [n1,n2,n3,n4] 
--  print seq
    mastermindAux seq 

mastermindAux :: [Int] -> IO ()
mastermindAux seq = do 
    putStr "Introduza 4 números de 1 a 9, espaçados: "
    nums <- getLine 
    let separados = words nums
        numeros = map (\ x -> read x :: Int) separados
        intersecao = numeros `intersect` seq 
        (certos,errados) = valoresCertos seq intersecao numeros 
    putStrLn ("Valores certos na posição certa: " ++ show certos) 
    putStrLn ("Valores certos na posição errada: " ++ show errados)
    if certos == 4 
    then putStrLn "Acertou em todos os números, PARABÉNS!!!" 
    else mastermindAux seq 

-- Função que devolve o número de valores na posicao certa e o numero de valores na posicao errada
valoresCertos :: [Int] -> [Int] -> [Int] -> (Int,Int)
valoresCertos seq intersecao introduzido = 
    somaPares $ map (\ n -> if null (elemIndices n seq `intersect` elemIndices n introduzido) -- se os indices de n forem iguais em ambas as listas, então o valor está nas posições corretas, indices porque n pode ser repetido 
                            then (0,1)
                            else (1,0) ) intersecao 

-- Função que soma a primeira componente e segunda componente de uma lista de pares. Ex: [(1,2),(4,5)] = (1+4,2+5)
somaPares :: [(Int,Int)] -> (Int,Int)
somaPares list = (comp1,comp2)
    where comp1 = sum $ map fst list 
          comp2 = sum $ map snd list 

