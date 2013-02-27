l = [1, 2, 3, 4, 5, 6, 7]

myLast (x:xs) = if xs == [] then x else myLast xs
myButLast (x:xs) = if xs == [] then [] else x : myButLast xs

elementAtHelper (x:xs) k i = if i == k then x else elementAtHelper xs k (i + 1)
elementAt lst k = elementAtHelper lst k 1

myLength [] = 0
myLength (x:xs) = 1 + myLength xs

myPrepend lst x = x : lst -- before I learned of flip :)
myReverse lst = foldl myPrepend [] lst