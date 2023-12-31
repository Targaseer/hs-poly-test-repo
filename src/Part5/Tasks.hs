module Part5.Tasks where

import Util(notImplementedYet)

-- Реализуйте левую свёртку
myFoldl :: (b -> a -> b) -> b -> [a] -> b
myFoldl oper acc [] = acc
myFoldl oper acc (h:t) = myFoldl oper (oper acc h) t

-- Реализуйте правую свёртку
myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr oper init [] = init
myFoldr oper init (h:t) = oper h (myFoldr oper init t) 

-- Используя реализации свёрток выше, реализуйте все остальные функции в данном файле

myMap :: (a -> b) -> [a] -> [b]
myMap f lst = myFoldr (\h t -> (f h):t) [] lst

myConcatMap :: (a -> [b]) -> [a] -> [b]
myConcatMap f lst = myConcat (myMap f lst)

myConcat :: [[a]] -> [a]
myConcat packedL = myFoldl (++) [] packedL

myReverse :: [a] -> [a]
myReverse lst = myFoldl (\t h -> h:t) [] lst

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter p lst = myConcatMap (\h -> if (p h) then [h] else []) lst

myPartition :: (a -> Bool) -> [a] -> ([a], [a])
myPartition p lst = ((myFilter p lst), (myFilter (\x -> not (p x)) lst))