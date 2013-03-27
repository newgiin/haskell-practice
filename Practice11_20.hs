module Practice11_20
    where

import Data.List

data Stuff a = Multiple Int a | Single a deriving Show

count [x] = Single x
count xs = Multiple (length xs) (head xs)
encodeModified xs = map count $ group xs 

decodeModified = concatMap 
    (\x -> case x of {Single a -> [a]; Multiple c a -> replicate c a})
    
-- #13 ??

dupli = concatMap (\x -> replicate 2 x)
repli xs n = concatMap (\x -> replicate n x) xs

-- #16
dropEvery' [] _ _ = []
dropEvery' (x:xs) n i 
    | i == n    = dropEvery' xs n 1
    | otherwise = dropEvery' xs n (i + 1)
    
dropEvery xs n = dropEvery' xs n 1

-- # 17
split' :: Int -> [a] -> [a] -> ([a], [a])
split' _ acc [] = (acc, [])
split' 0 acc l@(x:xs) = (acc, l)
split' n acc l@(x:xs) =  split' (n - 1) (acc ++ [x]) xs 
        
split :: [a] -> Int -> ([a], [a])
split xs n | n >= 0    = split' n [] xs
           | otherwise = split' ((length xs) + n) [] xs 

-- # 18
slice :: [a] -> Int -> Int -> [a]
slice xs a b = fst (split (snd (split xs (a - 1))) (b - a + 1)) 

-- # 19
rotate :: [a] -> Int -> [a]
rotate xs n = zs ++ yz where (yz, zs) = split xs n
     
-- # 20
removeAt :: Int -> [a] -> (a, [a])
removeAt n (x:xs) | n == 1    = (x, xs)
                  | otherwise = (e, x:ys) 
                      where (e, ys) = removeAt (n - 1) xs
