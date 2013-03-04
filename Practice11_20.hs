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
dropEveryHelper (x:xs) n i =
    if i == n 
        then dropEveryHelper xs n 1
        else x : dropEveryHelper xs n (i + 1)
dropEvery xs n = dropEveryHelper xs n 1