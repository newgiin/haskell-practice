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
dropEveryHelper [] _ _ = []
dropEveryHelper (x:xs) n i 
    | i == n    = dropEveryHelper xs n 1
    | otherwise = dropEveryHelper xs n (i + 1)
    
dropEvery xs n = dropEveryHelper xs n 1

-- # 17
splitHelper :: Int -> [a] -> [a] -> ([a], [a])
splitHelper _ acc [] = (acc, [])
splitHelper 0 acc l@(x:xs) = (acc, l)
splitHelper n acc l@(x:xs) =  splitHelper (n - 1) (acc ++ [x]) xs 
        
split :: [a] -> Int -> ([a], [a])
split xs n | n >= 0    = splitHelper n [] xs
           | otherwise = splitHelper ((length xs) + n) [] xs 

-- # 18
slice :: [a] -> Int -> Int -> [a]
slice xs a z = fst (split (snd (split xs (a - 1))) (z - a + 1)) 

-- # 19
rotate :: [a] -> Int -> [a]
rotate xs n = zs ++ yz where (yz, zs) = split xs n
     
-- # 20
removeAt :: Int -> [a] -> (a, [a])
removeAt n (x:xs) | n == 1    = (x, xs)
                  | otherwise = (e, x:ys) 
                      where (e, ys) = removeAt (n - 1) xs


