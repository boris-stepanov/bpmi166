{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fdefer-typed-holes #-}

module S2.Church where

import Prelude

-- Задача: построить тип данных для чисел Чёрча и `instance Num (Church a)`
-- https://en.wikipedia.org/wiki/Church_encoding
-- Для отладки полезно написать также `instance Show (Church Int)`. (Потребуется расширение FlexibleInstances)

newtype ChurchNum a = ChurchNum a -- Stub

instance Show (ChurchNum Int) where
instance Eq (ChurchNum Int) where
instance Num (ChurchNum a) where

-- Подобным образом построить тип пара

newtype ChurchCons a = ChurchCons a -- Stub

cons :: a -> a -> ChurchCons a
cons = _
first :: ChurchCons a -> a
first = _
second :: ChurchCons a -> a
second = _

instance (Show a) => Show (ChurchCons a) where

-- *** Сделать такую же пару, но из разнородных элементов. (Возможно, понадобится расширение RankNTypes)
