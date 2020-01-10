-- Комментарий

-- Подключаем расширение языка
{-# LANGUAGE InstanceSigs #-}
-- Флаги компилятора для отдельного файла
{-# OPTIONS_GHC -fdefer-typed-holes #-}

-- Объявление модуля
module Hello.Prim (
   add3    -- Явный экспорт
 ) where

-- Импорты всегда в заголовке файла
import           Prelude                     -- импортируем всё
import qualified Prelude as P                -- P.print, P.read
import           Prelude (print)             -- импортируем только `print`
import           Prelude hiding (undefined)  -- импортируем всё кроме `undefined`

-- Примитивы:
-- Числа: 0, 1, (-1). 0xff, 0o77,
-- Символы: 'a', 'ъ', '\1098', '\xaaa', '\o777' (utf8)
-- Строки: "abc", "", "\10\&\98", "\&" (пустая строка),
s = "multi \
 \line\
     \ string"
-- Кортежи: (1,2), (1, 'a', "abc")
-- Гомогенные списки: [], [1,2,3], [1..5], [1..], [1,3,..10] (только для Enum типов), 1:[], 1:2:3:[4,5,6]
-- Unit: ()
-- Functions: имеют тип (x -> y)
-- "abc" == ['a', 'b', 'c']

-- Объявляем константу
constant :: Int     -- Сигнатура
constant = 5        -- Значение

-- Функция от трёх аргументов
add3 :: Int -> Int -> Int -> Int
add3 a b c = a + b + c

-- Полиморфная функция (принимает любой тип)
const :: a -> b -> a
const a b = a

-- Point-free notation
add2and2 :: Int -> Int -> Int
add2and2 = add3 2

-- Pattern matching
head :: [a] -> a
head [] = error "empty list"
head (x:_) = x

-- Lambda
add2 :: Int -> Int -> Int
add2 = \a b -> a + b

-- Локальные переменные (let)
add3' :: Int -> Int -> Int -> Int
add3' a b c = let sumAB = a + b
                  sumABC = sumAB + c
              in  sumABC

-- Локальные переменные (where)
add3'' :: Int -> Int -> Int -> Int
add3'' a b c = sumABC
  where
  sumABC = sumAB + c
  sumAB = a + b

-- Конструкция if then else
isPositive :: Int -> Bool
isPositive a = if a > 0 then True else False

-- guards
sign :: Int -> Int
sign x
 | x > 0 = 1
 | x == 0 = 0
 | otherwise = 1    -- otherwise = True в Prelude

-- case
fib :: Int -> Int
fib x = case x of
  0 -> 1
  n -> n * fib (n - 1)

-------------------------------------- Типы

-- Объявление тип
data NewType = Cons1 | Cons2 Int Char | NewType

-- Полиморфный, рекурсивный тип
data List a = Nil | Cons a (List a)

-- Класс типов
class MyEq a where
  eq :: a -> a -> Bool

-- Инстансы класса
instance MyEq NewType where
  Cons1 `eq` Cons1 = True
  NewType `eq` NewType = True
  _ `eq` _ = False

-- Полиморфная функция
isElem :: MyEq a => a -> [a] -> Bool
isElem _ [] = False
isElem x (y:ys) = if x `eq` y then True else isElem x ys

------------------------------------ Задачи

-- Функция разворота списка
reverse :: [a] -> [a]
reverse = _

-- Двоичное дерево
data Tree a

-- Обход двоичного дерева
traverse :: Tree a -> [a]             --         4
traverse = _                          --       2   5        --> [1,2,3,4,5,6]
                                      --      1 3   6
                                      -- (++) :: [a] -> [a] -> [a]
-- Хвосторекурсивный факториал
-- Проверить с помощью `seq (trueFact 10000) "DONE"`
trueFact :: Int -> Int
trueFact = _

-- *Хвосторекурсивные числа Фибоначчи
-- Проверить с помощью `seq (trueFib 100000) "DONE"`
trueFib :: Int -> Int
trueFib = _

-- **Последовательность чисел Фибоначчи
-- zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
-- tail :: [a] -> [a]
-- Проверить с помощью `take 20 fibs`
fibs :: [Int]
fibs = 1:1:_    -- (некоторая функция от facts)

-- **Числа Пеано
data Nat

instance Eq Nat where
  (==) = _

instance Num Nat where
  (+) = _
  (*) = _
  fromInteger :: Integer -> Nat
  fromInteger = _
