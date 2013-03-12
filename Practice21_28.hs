import Practice11_20
import System.Random

insertAt :: a -> [a] -> Int -> [a]
insertAt e xs n = ys ++ [e] ++ zs where (ys, zs) = Practice11_20.split xs (n-1)

range :: Int -> Int -> [Int]
range a b |  a == b   = [b]
          | otherwise = a : range (a+1) b
          
-- rndSelect xs n 
    -- | n == 0    = []
    -- | otherwise = e : rndSelect (n - 1) ys 
        -- where (e, ys) = removeAt p xs 
        -- where p <- randomRIO (1::Int, length xs)
        
-- rndTest = p + 1 where p <- randomRIO (1::Int, 6)

-- #26
combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations 1 [x] = [[x]]
combinations n [x] = []
combinations n (x:xs) = 
    (map ((:) x) (combinations (n-1) xs)) ++ combinations n xs
