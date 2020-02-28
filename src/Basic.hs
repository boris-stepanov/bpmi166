{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE KindSignatures #-}
{-# OPTIONS_GHC -fdefer-typed-holes #-}

module Basic where

import           Prelude (Bool(..))
import qualified Control.Monad
import           Data.Function (flip, ($), (&), const)
import           System.IO

-- https://wiki.haskell.org/Typeclassopedia
-- Нужные нам типы

newtype Identity a = Identity { runIdentity :: a }
data Tree a = Tip | Node a [Tree a]

-------------------------------- Категория ---------------------------------

{-
https://wiki.haskell.org/Hask
Hask - категория (*, ->), в которой объектами являются алгераические типы Хаскелля, а морфизмы - функции.

Control.Category:
-}
class Category (cat :: * -> * -> *) where
  id :: cat a a
  (.) :: cat b c -> cat a b -> cat a c

{-
Законы:
∀ f => f . id == id . f == f            - нейтральный элемент
∀ f g ∃ h: h = f . g, h x = f (g x)     - композиция
-}

instance Category (->) where
  id = _
  (.) = _

-------------------------------- Функтор -----------------------------------

{-
Функторы - преобразования категорий, уважающие стрелки. Поскольку любая категория в Хаскелле состоит из типов Хаскелля, то есть является подкатегорией Hask, то и любой функтор является эндофунктором. В Хаскелле класс функтор - это `X :: * -> *`, для которого существует эндофунктор `Hask -> Type`.

Data.Functor
-}
class Functor (f :: * -> *) where
  fmap :: (a -> b) -> (f a -> f b)

-- часто используется слово lift для функций, "поднимающих" нормальные функции в новую категорию. Здесь это fmap.

{-
Законы:
fmap id = id
fmap (f . g) = fmap f . fmap g

Можно доказать, что для любого типа существует не более одной реализации экземпляра Functor, удовлетворяющей законам. Ссылки на доказательство есть в статье Typeclassopedia выше.
-}

-- Полезные функции:
(<$>) :: Functor f => (a -> b) -> f a -> f b
(<$>) = fmap

(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip fmap

void :: Functor f => f a -> f ()
void = fmap (const ())

-- Задание:
instance Functor Identity where
instance Functor ((,) a) where
instance Functor ((->) a) where
instance Functor Tree where

-------------------------------- Аппликатор --------------------------------

{-
Аппликаторы - это класс типов, допускающий лифтинг точек и применение поднятых функций к поднятым точкам. Аппликаторы могут иметь больше одного определения.
Control.Applicative:
-}
class Functor f => Applicative f where
  {-# MINIMAL pure, ((<*>) | liftA2) #-}
  pure :: a -> f a
  liftA2 :: (a -> b -> c) -> (f a -> f b -> f c)
  (<*>) :: f (a -> b) -> f a -> f b
  liftA2 f x = (<*>) (fmap f x)
  (<*>) = liftA2 id

{-
Законы:
pure id <*> v = v
pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
pure f <*> pure x = pure (f x)
u <*> pure y = pure ($ y) <*> u
-}

-- Полезные функции:
(*>) :: Applicative f => f a -> f b -> f b
(*>) = liftA2 (const id)
-- (<*)

when :: Applicative f => Bool -> f () -> f ()
when pred run = if pred then run else pure ()
-- unless

-- Задание:
instance Applicative Identity where
instance Applicative ((,) a) where
instance Applicative ((->) a) where
instance Applicative Tree where

-------------------------------- Монада ------------------------------------

{-
Монады в Хаскелле являются подклассом Applicative, что не является математически корректным, но было сделано ради оптимизации. Есть два определения монад, но в определении класса в стандартной библиотеке зафиксировано через моноид эндофункторов.
Control.Monad:
-}

class Applicative f => Monad f where
  (>>=) :: f a -> (a -> f b) -> f b      -- он же bind

{-
Законы:
pure a >>= k = k a
m >>= pure = m
m >>= (\x -> k x >>= h) = m >>= k >>= h
-}

-- Полезности:
return :: Monad m => a -> m a
return = pure

(>>) :: Monad m => m a -> m b -> m b
(>>) = (*>)

(>=>) :: Monad m => (a -> m b) -> (b -> m c) -> (a -> m c)
f >=> g = \x -> f x >>= g

forever :: Monad m => m a -> m b
forever x = x >> forever x

-- Задания:

-- Определить join через bind, pure, fmap
join :: Monad m => m (m a) -> m a
join = _

-- Определить bind через join, pure, fmap. Здесь bind - синоним (>>=)
bind :: Monad m => m a -> (a -> m b) -> m b
bind = _

{-
Таким образом, Монада из теории категорий это
Тройка (m, return, join), для которой коммутативны некоторые схемы
ЛИБО
Моноид (return, >=>)
-}

instance Monad Identity where
instance Monad ((,) a) where
instance Monad ((->) a) where
instance Monad Tree where

{-
Категория Клейсли
-}
newtype Kleisli m a b = Kleisly (a -> m b)

instance Monad m => Category (Kleisli m) where
  id = _
  (.) = _

-------------------------------- Альтернатор -------------------------------

{-
Control.Applicative
-}
class Applicative f => Alternative f where
  empty :: f a
  (<|>) :: f a -> f a -> f a
  some :: f a -> f [a]
  many :: f a -> f [a]

guard :: Alternative f => Bool -> f ()
guard True = pure ()
guard False = empty

-------------------------------- Сахарок -----------------------------------

{-
do-syntax:
-}

main :: IO ()
main = Control.Monad.forever $ do
  putStrLn "Awaiting for input"
  char <- getChar
  putStrLn "Received one char:"
  print char

{-
do-syntax - серия инструкций разделённых переносами строк либо записанная в виде
do { cmd1; cmd2; cmd3 ... }

Каждая инструкция имеет вид либо
  1. methodCall
  2. var <- methodCall
  3. let var = pureValue

По определению, это раскрывается следующим образом:

 1. desugar
      methodCall
      rest

        ||
        \/

      methodCall >> desugar[rest]
 2. desugar
      var <- methodCall
      rest

        ||
        \/

      methodCall >>= (\var -> desugar[rest])
 3. desugar
      let var = value
      rest

        ||
        \/

      let var = value in desugar[rest]

  ЗАМЕЧАНИЕ!!!
  return - не завершает вызов функции, не выходит из контекста, не передаёт никуда значение. В коде ниже, return не делает ровным счётом ничего.

```
  do
    print "Hello"
    return 42
    print "Hello again"
```
  return нужен, чтобы прямо сейчас сохранить значение, если не хочется использовать let, или как последний вызов, и тогда результат действительно будет передан наверх.
-}

{-
Теперь мы можем понять, что делает list comprehension. С расширением MonadComprehensions, его можно использовать для произвольных монад.

[ F x y | x <- method1, predicate1 x, y <- method2, predicate2 x y ]

 это то же самое, что
```
foo = do
  x <- method1
  guard $ predicate x
  y <- method2
  guard $ predicate x y
  return $ F x y
```
-}

{-
Задание на дом:
1. Зная законы для перечисленных классов, написать quickcheck тесты для всех самописных instance'ов.

2. Разобраться, как работает встроенный Applicative для списков, придумать другой алгоритм свётрки и написать свой. Сделать тесты.
-}

newtype WrappedList a = WL [a]
instance Functor WrappedList where
instance Applicative WrappedList where
