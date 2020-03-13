{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fdefer-typed-holes #-}

module Basic where

import           Prelude
import qualified Control.Monad
import           Data.Function (flip, ($), (&), const)
import           System.IO

-- https://wiki.haskell.org/Typeclassopedia

-------------------------------- RWS ---------------------------------------

{-
Три популярные монады:
-}

newtype Reader r a = Reader { runReader :: r -> a }
newtype Writer w a = Writer { runWriter :: (a, w) }
newtype State s a = State { runState :: s -> (a, s) }

-------------------------------- Reader ------------------------------------

{-
Reader - монада в которой можно в любой момент "попросить" контекст. Монадическое действие можно запустить для заданного контекста с помощью того же runReader.
-}

instance Functor (Reader r)
instance Applicative (Reader r)
instance Monad (Reader r)

-- Вызовем поддействие для изменённого контекста.
withReader :: (r' -> r) -> Reader r a -> Reader r' a
withReader = _

-- Действие, возвращающее значение контекста
ask :: Reader r r
ask = _

-- Например: факториал через Reader.
factorialReader :: Integer -> Integer
factorialReader n = runReader (go 1) n
  where
  go :: Integer -> Reader Integer Integer
  go acc = do
    i <- ask
    if i > 0 then withReader pred (go $ acc * i) else return acc

-------------------------------- Writer ------------------------------------

{-
Writer - монада, которая "копит" сообщения в моноиде w.
-}

instance Functor (Writer w)
instance Monoid w => Applicative (Writer w)
instance Monoid w => Monad (Writer w)

-- Достаёт накопленные данные, без самого результата.
execWriter :: Writer w a -> w
execWriter = _

-- Преобразовывает данные от другого поддействия.
mapWriter :: ((a,w) -> (b,w')) -> Writer w a -> Writer w' b
mapWriter = _

-- Прибавляет новое значение к накопителю.
tell :: w -> Writer w ()
tell = _

-- Извлекает накопленные на данный момент данные.
listen :: Writer w a -> Writer w (a, w)
listen = _

-- Например: факториал через Writer. Нам понадобится Monoid, суммирующий инты.

newtype Prod = Prod { getProd :: Integer }        deriving (Eq, Num)
instance Semigroup Prod where
  Prod a <> Prod b = Prod $ a * b

instance Monoid Prod where
  mempty = Prod 1

factorialWriter :: Integer -> Integer
factorialWriter n = getProd $ execWriter (go (Prod n))
  where
  go 1 = pure ()
  go n = tell n >> go (n - 1)

-- А вообще Writer часто используют для логгирования - просто сбрасываем отладочные данные в список строк.

-------------------------------- State -------------------------------------

-- State - расширенный Reader, в котором можно менять контекст более понятным образом.

instance Functor (State s)
instance Applicative (State s)
instance Monad (State s)

execState :: State s a -> s -> s
execState = _

evalState :: State s a -> s -> a
evalState = _

-- Извлечь данные из контекста (как в ask)
get :: State s s
get = _

-- Положить данные в контекст
put :: s -> State s ()
put = _

-- Применить функцию к контексту
modify :: (s -> s) -> State s ()
modify = _

-- Как всегда - факториал. Будем хранить в контексте и счётчик и результат.
factorialState :: Integer -> Integer
factorialState n = snd $ execState go (n, 1)
  where
  go = do
    (k, acc) <- get
    if k == 0 then return () else put (k - 1, acc * k) >> go

-------------------------------- ST/IO -------------------------------------

-- https://hackage.haskell.org/package/base/docs/Control-Monad-ST.html

{-
ST - особая монада, разновидность State, но позволяющая работать с памятью in place. В модулях Data.STRef, Data.Array.ST (библиотека Array) определены реальные мутабельные переменные и массивы. Хотя ST разрешает мутировать данные в памяти, но она гарантирует, что мы не сможем, например, выйти в сеть, в файловую систему, и что мутабельные данные определены на уровне типа.
-}

{-
Чтобы в чистом языке работать с "нечистыми" данными - временем, пользовательским вводом и т.д. мы можем объявить их просто как ещё один поток событий. По определению type `IO = ST RealWorld` - это state-монада, которая мутирует реальный мир. (Библиотека ghc-prim)

getLine :: IO String
getLine :: ST RealWorld String
getLine ~~ RealWorld -> (String, RealWorld)

аналогично
print ~~ String -> (RealWorld -> ((), RealWorld))
-}

-------------------------------- Concurrency -------------------------------

{-
Хаскелл использует "зелёные" потоки - это потоки, которые управляются runtime'ом компилятора, а не операционной системой. Логически для программиста - это обычные легковесные потоки (запустить 10000 потоков - вообще не проблема и никем не осуждается), а физически - больше похоже на асинхронную систему. С включенным флагом -threaded ghc может запускать свои потоки на реальных OS потоках, возможно, мигрируя между ними. Это поведение может быть настроено. Для работы с конкуррентностью нам нужен модуль Control.Concurrent и функции forkIO, forkFinally.
-}

{-
Для синхронизации потоков можно использовать Data.IORef - общие мутабельные переменные, Control.Concurrent.MVar - "mutable location", которое может хранить значение или не хранить ничего. Вызовы putMVar, takeMVar могут блокироваться в зависимости от наличия данных. Хаскелл пытается отслеживать deadlock'и, но полагаться на это не следует.
-}

-------------------------------- Exeptions ---------------------------------

{-
Control.Exception.
В Хаскелле асинхронные исключения - любой поток может бросить любое исключение в любой другой поток, имея его ThreadId. Для работы с исключениями используются функции try, catch, handle, mask, killThread, throw, throwIO. По умолчанию, при использовании catch-like функций следует указывать тип исключения, которое мы хотим поймать:

catch someAction (\(e :: IOException) -> doSmth e)
такой код обработает IOException, но все остальные пробросит выше. Чтобы ловить все исключения следует использовать обобщающий SomeException

catch someAction (\(e :: SomeException) -> doSmth e)
такой код обработает все исключения.
-}
