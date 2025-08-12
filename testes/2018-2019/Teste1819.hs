
module Teste1819 where


elemIndices :: Eq a => a -> [a] -> [Int]
elemIndices = undefined

isSubsequenceOf :: Eq a => [a] -> [a] -> Bool
isSubsequenceOf = undefined


data BTree a = Empty | Node a (BTree a) (BTree a)

lookupAP :: Ord a => a -> BTree (a,b) -> Maybe b
lookupAP _ Empty = Nothing
lookupAP x (Node y left right) | x == fst y = snd y
                               | x < fst y = lookupAP x right
                               | otherwise = lookupAP x left


zipWithBT :: (a -> b -> c) -> BTree a -> BTree b -> BTree c
zipWithBT = undefined

digitAlpha :: String -> (String,String)
digitAlpha = undefined


data Seq a = Nil | Cons a (Seq a) | App (Seq a) (Seq a)

firstSeq :: Seq a -> a
firstSeq = undefined

dropSeq :: Int -> Seq a -> Seq a
dropSeq = undefined

instance Show Seq where
    show = showSeq

showSeq :: Seq a -> String
showSeq = undefined


type Mat a = [[a]]

getElem :: Mat a -> IO a
getElem = undefined

magic :: Mat Int -> Bool
magic = undefined

