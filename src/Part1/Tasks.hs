module Part1.Tasks where

import Util(notImplementedYet)

-- синус числа (формула Тейлора)
mySin :: Double -> Double
mySin = notImplementedYet

-- косинус числа (формула Тейлора)
myCos :: Double -> Double
myCos = notImplementedYet

-- наибольший общий делитель двух чисел
myGCD :: Integer -> Integer -> Integer
myGCD a b = if b /= 0 then abs (myGCD b (mod a b)) else abs a

-- является ли дата корректной с учётом количества дней в месяце и
-- вискокосных годов?
isDateCorrect :: Integer -> Integer -> Integer -> Bool
isDateCorrect = notImplementedYet

-- возведение числа в степень, duh
-- готовые функции и плавающую арифметику использовать нельзя
myPow :: Integer -> Integer -> Integer
myPow a 0 = 1
myPow a 1 = a
myPow a n = a * myPow a (n - 1)

-- является ли данное число простым?
isPrime :: Integer -> Bool
isPrime 1 = True
isPrime 2 = True
isPrime x = isPrimeAcc x (floor (sqrt (fromInteger x)))
isPrimeAcc :: Integer -> Integer -> Bool
isPrimeAcc x 1 = True
isPrimeAcc x curr = if (rem x curr) == 0 then False else isPrimeAcc x (curr - 1)

type Point2D = (Double, Double)

-- рассчитайте площадь многоугольника по формуле Гаусса
-- многоугольник задан списком координат
shapeArea :: [Point2D] -> Double
shapeArea points = notImplementedYet

-- треугольник задан длиной трёх своих сторон.
-- функция должна вернуть
--  0, если он тупоугольный
--  1, если он остроугольный
--  2, если он прямоугольный
--  -1, если это не треугольник
triangleKind :: Double -> Double -> Double -> Integer
triangleKind a b c = notImplementedYet
