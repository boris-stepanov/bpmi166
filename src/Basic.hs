{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# OPTIONS_GHC -fdefer-typed-holes #-}

module Basic where

import Prelude (id, seq, Int, Num(..), Monoid(..), Ord(..), Bool(..), Maybe(..))

{-
Задачки: написать разные способы свёртки списка.
-}

-- Свёртка списка с хвоста. Проверить, что `foldr (+) 0 [0 .. 30_000_000]` сломается со stack overflow.
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr = _

-- Свёртка списка с начала. Проверить, что `foldl (+) 0 [0 .. 30_000_000]` сломается со stack overflow.
foldl :: (b -> a -> b) -> b -> [a] -> b
foldl = _

-- Понять, почему ленивая свёртка слева так просто не работает и с помощью seq написать эффективную свёртку. Проверить, что `foldl' (+) 0 [0 .. 30_000_000]` посчитается за пару секунд.
foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' = _

-- Умножение, ленивое по первому аргументу.
(?) :: Int -> Int -> Int
(?) _ 0 = 0
(?) a b = a * b

-- Зачем вообще нужны foldl, foldr? Сравните вызов `foldr(l,l') (?) 1 [1,2,undefined,0]`

{-
```
class Foldable t where
  foldMap :: Monoid m => (a -> m) -> t a -> m
  foldr :: (a -> b -> b) -> b -> t a -> b
  {-# MINIMAL foldMap | foldr #-}
```
-}

{-
Избавляемся от ленивости:
seq :: a -> b -> a
-}

-- По определению
($!) :: (a -> b) -> a -> b
f $! x = x `seq` f x

-- В WHNF StrictData содержит `a` также в WHNF
data StrictData a b = StrictData !a b

-- StrictData undefined () - _|_
-- StrictData (1, undefined) () - нe _|_

-- Ленивые и энергичные паттерны

-- x будет приведён к WHNF, несмотря на то, что он не используется. (+расширение BangPatterns)
strictPattern :: Int -> ()
strictPattern !x = ()

-- Первый шаблон автоматически пройдёт сопоставление. Если аргумент, на самом деле Nothing - получим исключение
-- Второй шаблон не выполнится никогда, а компилятор покажет предупреждение.
lazyPattern :: Maybe Int -> Int
lazyPattern ~(Just x) = x
lazyPattern x = 0

-- Фантомные переменные типа - это переменные, которые не участвуют в конструкторах данных:
data NotMaybe a b = NotNothing | NotJust a
-- b - фантомная переменная, все типы в семействе `NotMaybe a *` имеют одинаковые точки,
-- но компилятор всё равно будет проверять типы. Тип точки `NotNothing` не всегда равен типу точки `NotNothing`

-- Пример из Control.Applicative
data Const a b = Const a
-- Пример из Data.Proxy
data Proxy a = Proxy

-- Record syntax:

data Foo a = Foo { getA :: a, getInt :: Int } | Bar { getInt :: Int }

-- Тип эквивалентен типу `Foo a Int | Bar Int`, но у нас появились геттеры `getA`, `getInt`
-- :t getA :: Foo a -> a
-- И намного больше:

foo :: Foo a -> Int
foo Foo { getInt = x } = x
foo Bar { getInt = x } = x

setIntInFoo :: Foo a -> Int -> Foo a
setIntInFoo foo x = foo { getInt = x }

-- Также в pattern-matchinge при помощи @ можно добавлять биндинги к разным группам:

bar :: Foo a -> Foo a
bar x@(Foo y z) = if z > 0 then x { getInt = 5 } else x


-- Другой синтаксис для типов

-- буквально синоним типа, при разборе программы заменяется на `Foo Int` везде.
-- Мы уже видели, что по определение `type String = [Char]`
type AnotherFoo a = Foo a
type PhantomFoo a b = Foo a
type IntFoo = Foo Int

{-
Вообще-то создание типа при помощи data так же добавляет новый указатель.
Теперь, чтобы добраться до Int'а в глубине Foo, машине придётся сначала переходить по указателю FooBoxed, затем по Foo/Bar.
Есть расширения, позволяющие распаковать (unbox) внутренние данные, или можно воспользоваться newtype.
-}

data FooBoxed a = FooBoxed (Foo a)
newtype FooUnwrapped a = FooUnwrapped (Foo a)

{-
FooUnwrapped - новый тип и все особенности типов Хаскелла для него работают так же. Но после компиляции FooUnwrapped исчезает, оставляя только перегруженные функции.
newtype можно использовать не всегда - он должен иметь строго один конструктор данных с одним аргументом. То есть по сути только `<Mark> <OtherType>`.
На конструктор типа ограничений нет.
-}

-- Вопрос: почему в базовой библиотеке Хаскелля нет определения `instance Semigroup Int where...`?
-- Подсказка: попробуйте его определить самостоятельно.
