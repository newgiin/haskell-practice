module Main
    where

import Practice11_20
import System.Random
import Control.Monad (replicateM)
import Data.List
import qualified Data.Map as Map

insertAt :: a -> [a] -> Int -> [a]
insertAt e xs n = ys ++ [e] ++ zs where (ys, zs) = Practice11_20.split xs (n-1)

range :: Int -> Int -> [Int]
range a b |  a == b   = [b]
          | otherwise = a : range (a+1) b

{-
- #23 For problems involving Random, I looked at the solution beforehand
- to learn the use of monads.
-}
rndSelect :: [a] -> Int -> IO [a]
rndSelect [] _ = return []
rndSelect _ 0 = return []
rndSelect l n
    | n < 0 = error "N must be greater than zero."
    | otherwise = do 
        p <- randomRIO (1, (length l))
        let (e, rest) = removeAt p l
        rest' <- rndSelect rest (n-1)
        return $ e:rest'

-- #24
diffSelect :: Int -> Int -> IO [Int]
diffSelect n m
    | n < 0 = error "N must be greater than zero."
    | otherwise = do nums <- replicateM n $
                               randomRIO (0::Int, m)
                     return nums

-- # 25
rndPermu :: [a] -> IO [a]
rndPermu xs = rndSelect xs (length xs)
             
-- #26
combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations 1 [x] = [[x]]
combinations n [x] = []
combinations n (x:xs) = 
    (map ((:) x) (combinations (n-1) xs)) ++ combinations n xs
    
-- #27
myGroup :: Eq a => [Int] -> [a] -> [[[a]]]
myGroup (c:[]) xs = [[xs]]
myGroup (c:cs) xs =
    concat (map (\ys -> map (\zs -> ys:zs) (myGroup cs (xs\\ys)) ) (combinations c xs))

-- #28a
lenCompare xs ys | (length xs) < (length ys) = LT
    | (length xs) > (length ys) = GT
    | otherwise                 = EQ
lsort :: [[a]] -> [[a]]
lsort = sortBy lenCompare

-- #28b
f m xs = Map.insertWith (++) (length xs) [xs] m
lfsort :: [[a]] -> [[a]]
lfsort xs = concat (lsort (Map.elems (foldl f Map.empty xs)))