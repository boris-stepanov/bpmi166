{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fdefer-typed-holes #-}

module S2.Basic where

import Prelude (Maybe(..), Either(..), Bool)

{-
Базовые типы

data Bool = True | False - по определению

data () = ()

Int - знаковый целый, приблизительно 8-ми байтный
Integral - знаковый целый безразмерный
Float, Double - числа с плавающей точкой

Char - Юникодовый символ
type String = [Char] - по определению. Списки медленные, для производительности следует использовать типы из библиотек bytestring и text

-}

{-
Важные базовые типы

data Maybe a = Nothing | Just a

data Either e a = Left e | Right a

-}

{-
Базовые классы

Eq a - элементы множества можно сравнивать при помощи (==) и (/=)

(Eq a) => Ord a - элементы множества можно сравнивать при помощи >, <. >=, <=
data Ordering = EQ | LT | GT
compare :: Eq a => a -> a -> Ordering  - по определению

Bounded a - ограниченные множества, определены функции minBound, maxBound

Enum a - перечислимые типы, можно подставлять в списки вроде [x..y]

Show a - данные, которые можно конветировать в строки
Read a - данные, которые можно читать из строки
Для автоматически выведенных инстансов гарантирует, что `read . show == id`
-}

{-
Базовые числовые классы

Num a - кольцо целых чисел
Real a, Fractinal a, RealFrac a, RealFloat a - различные виды полей
Integral a - евклидово кольцо, есть деление с остатком
Floating a - числа с определёнными pi, exp, log, sin...

-}

{-
Базовые алгебраические классы

Semigroup a - полугруппы, множества с ассоциативным сложением (<>) :: a -> a -> a
Semigroup a => Monoid a - моноид, полугруппа с нулём mempty

Foldable t - "сворачиваемые" данные, определены с помощью
foldMap :: Monoid m => (a -> m) -> t a -> m

Category arr - категория, определены функции id :: a -> a и композиция (.) :: (b -> c) -> (a -> b) -> (a -> c)

Эти типы будем разбирать, после того как пройдём теорию категорий на лекциях
Functor => Applicative => Monad
Traversable, Alternative

А ещё в Хаскелле есть list comprehension (который потом переняли в Python), но он опирается на монады, поэтому разберём его позже.
[(x, y) | x <- [1..10], y <- [1..10], x `mod` y == 0]
-}

-- Задачи - населить типы

flip :: (a -> b -> c) -> b -> a -> c
flip = _

until :: (a -> Bool) -> (a -> a) -> a -> a
until = _

map :: (a -> b) -> [a] -> [b]
map = _

filter :: (a -> Bool) -> [a] -> [a]
filter = _

fix :: (a -> a) -> a
fix = _
