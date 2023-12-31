module Part3.Tasks where

import Util (notImplementedYet)
import Data.List
import Data.Char
import Data.Function (on)

-- Функция finc принимает на вход функцию f и число n и возвращает список чисел [f(n), f(n + 1), ...]
finc :: (Int -> a) -> Int -> [a]
finc f n = map f [n..]

-- Функция ff принимает на вход функцию f и элемент x и возвращает список [x, f(x), f(f(x)), f(f(f(x))) ...]
ff :: (a -> a) -> a -> [a]
ff f x = x : map f (ff f x)

-- Дан список чисел. Вернуть самую часто встречающуюся *цифру* в этих числах (если таковых несколько -- вернуть любую)
mostFreq :: [Int] -> Int
mostFreq ints = fst (((sortBy (flip compare `on` snd)) (frequencies ints)) !! 0)
frequencies :: [Int] -> [(Int, Int)]
frequencies ints = map (\x -> (digitToInt (head x), length x)) (group (sort (concatMap show ints)))

-- Дан список lst. Вернуть список элементов из lst без повторений, порядок может быть произвольным.
uniq :: (Eq a) => [a] -> [a]
uniq lst = uniqAcc lst []
uniqAcc [] acc = acc
uniqAcc (hlst : tlst) acc = if elem hlst acc then uniqAcc tlst acc else uniqAcc tlst (hlst : acc)

-- Функция grokBy принимает на вход список Lst и функцию F и каждому возможному
-- значению результата применения F к элементам Lst ставит в соответствие список элементов Lst,
-- приводящих к этому результату. Результат следует представить в виде списка пар.
grokBy :: (Eq k) => (a -> k) -> [a] -> [(k, [a])]
grokBy f l = groupByFirst (zip (map f l) l) [] []
groupByFirst [] collected acc = acc
groupByFirst (hl : tl) collected acc = 
    if elem (fst hl) collected 
    then groupByFirst tl collected acc
    else groupByFirst tl ((fst hl) : collected) (((fst hl), (snd hl : (map snd (filter (\x -> fst x == fst hl) tl)))) : acc)