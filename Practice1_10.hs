l = [1, 2, 3, 4, 5, 6, 7]

myLast (x:xs) = if xs == [] then x else myLast xs
myButLast (x:xs) = if xs == [] then [] else x : myButLast xs

eentAtHelper (x:xs) k i = if i == k then x else eentAtHelper xs k (i + 1)
eentAt xs k = eentAtHelper xs k 1

myLength [] = 0
myLength (x:xs) = 1 + myLength xs

myPrepend xs x = x : xs -- before I learned of flip :)
myReverse xs = foldl myPrepend [] xs

isPalindrome xs = xs == (reverse xs)
-- # 7
data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List []) = []
flatten (List (x:xs)) = flatten x ++ flatten (List xs)

-- # 8
headEq :: (Eq a) => a -> [a] -> [a]
headEq x [] = [x]
headEq x acc = if x /= (head acc) then [x] ++ acc else acc

compress :: (Eq a) => [a] -> [a]
compress = foldr headEq []

-- # 9
headEq' :: (Eq a) => a -> [[a]] -> [[a]]
headEq' e [] = [[e]]
headEq' e (x:xs) = 
    if e /= (head x) 
        then [[e]] ++ (x:xs) 
        else [x ++ [e]] ++ xs

pack :: (Eq a) => [a] -> [[a]]
pack = foldr headEq' []

-- # 10
headEq'' :: Eq a => a -> [(Int, a)] -> [(Int, a)] 
headEq'' e [] = [(1, e)]
headEq'' e (x:xs) = 
    if e /= snd x
        then [(1, e)] ++ (x:xs)
        else [(1 + fst x, e)] ++ xs

encode :: Eq a => [a] -> [(Int, a)]
encode = foldr headEq'' [] 
