{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -fdefer-typed-holes #-}

module HW01 where

-- Общее для всех задач: все рекурсивные вызовы должны быть хвостовыми.

{-
1. Задача на лямбда-исчисление
1.1. Уберите скобки в следующем лямбда-терме, произвести редукцию. Расписать пошагово:
((λ p. (λ q. ((q (p r)) s))) ((q ((λ p. p) r)) s))
1.2. Аналогично:
((λ a. λ b. (λ x. x) b a (a b x) ((λ a. (λ b. a)) x)) (λ b. b)) [x := b]
-}

{-
2. Реализовать расширенный алгоритм Евклида: `euclid x y = (a, b, r)`, где `a*x + b*y = r`.
-}

euclid :: Integer -> Integer -> (Integer, Integer, Integer)
euclid = _

{-
3. Реализуйте функцию, считающую n-ое число Каталана:
https://en.wikipedia.org/wiki/Catalan_number
-}

catalan :: Integer -> Integer
catalan = _

{-
4. Не пользуясь стандартными функциями (арифметическими можно), сгенерируйте список всех степеней числа.
pows 2 = [1,2,4,8,...]
-}

pows :: Integer -> [Integer]
pows = _

{-
5. Написать функцию, раскладывающую число на список его простых сомножителей.
primesOf 12 = [2,2,3]
-}

primesOf :: Integer -> [Integer]
primesOf = _

{- 6. Заселить следующие типы термами: -}

-- # 6.1:

fun1 :: a -> b
fun1 = _

-- # 6.2:

fun2 :: (a -> b -> c) -> (a -> b) -> a -> c
fun2 = _

{-
7. Функции head и tail из библиотеки Data.List не определены на пустых списках.
-}

{-
7.1 Требуется реализовать тип данных "непустой список", который гарантированно не может иметь длину меньше 1. (Можно использовать дополнительный тип при необходимости).
-}

data NonEmptyList a

{-
7.2. Реализовать базовый функционал для типа "непустой список".
-}

lengthNonEmpty :: NonEmptyList a -> Int
lengthNonEmpty = _

headNonEmpty :: NonEmptyList a -> a
headNonEmpty = _

toListNonEmpty :: NonEmptyList a -> [a]
toListNonEmpty = _

reverseNonEmpty :: NonEmptyList a -> NonEmptyList a
reverseNonEmpty = _

{-
7.3. Реализовать базовые инстансы для типа "непустой список".
-}

instance Eq a => Eq (NonEmptyList a) where
instance Semigroup (NonEmptyList a) where

{-
8. Определим тип "непустое дерево":
-}

data Tree a = Node a [Tree a]    deriving (Show, Eq)

-- # 8.1 Реализовать instance Functor для деревьев

instance Functor Tree where
 fmap :: (a -> b) -> Tree a -> Tree b
 fmap = _

-- # 8.2. Реализовать функцию, которая возвращает список элементов дерева

treeToList :: Tree a -> [a]
treeToList = _

-- # 8.3 Реализовать проверку что дерево является кучей, то есть голова любого узла меньше его потомков.

isHeap :: Ord a => Tree a -> Bool
isHeap = _

-- # 8.4 Назовём кучу "потрясной", если все элементы k-го потомка не превосходят любой элементы k+1-го потомка. Реализуйте проверку на "потрясность"

isAmazingHeap :: Ord a => Tree a -> Bool
isAmazingHeap = _

{-
9. В стандартной библиотеке Хаскелля тип Int не является экземпляром классов Monoid и Semigroup, потому что целые числа являются моноидом по сложению и умножению одновременно.
-}

{-
9.1. Напишите типы-обёртки SumMonoid, ProdMonoid и соответствующие инстансы.
-}

data SumMonoid a
data ProdMonoid a

instance Num a => Semigroup (SumMonoid a) where
instance Num a => Monoid (SumMonoid a) where
instance Num a => Semigroup (ProdMonoid a) where
instance Num a => Monoid (ProdMonoid a) where

{-
9.2. Используя только конструкторы данных для `SumMonoid`, `ProdMonoid` и оператор `foldMap` из библиотеки Data.Foldable написать функции, суммирующие и перемножающие список целых чисел.
-}

sumAll :: [Int] -> Int
sumAll = _

prodAll :: [Int] -> Int
prodAll = _

{- 10. Lambda lifting - превращение свободных переменных в аргументы. Например:

```
pow :: Int -> Int -> Int
pow n = powTail 1
  where
  powTail acc 0 = acc
  powTail acc k = powTail (acc * n) (k - 1)
```

Результат вызова `powTail` зависит от `n`, поэтому мы не можем вынести `powTail` в таком виде наружу.
-}

-- # 10.1. Вынести `powTail` как глобальную функцию.

pow :: Int -> Int -> Int
pow n x = powTail _

powTail :: a
powTail = _

{- # 10.2
Сделать lambda lifting для `from0` и `from1` и вынести их как глобальные функции.

cyclic :: [Int]
cyclic = let from0 = 0:from1
             from1 = 1:from0
          in from0
-}

cyclic :: [Int]
cyclic = from0 _

from0 :: a
from0 = _

from1 :: a
from1 = _
