{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeOperators #-}

import Data.Kind (Type)

infixr 4 ~>

-- base types
type f ~> g = forall a. f a -> g a

data Unit :: Type where
  Unit :: Unit

data Bool :: Type where
  True  :: Bool
  False :: Bool

-- 'Data' classes
class Eq a where
  eq :: a -> a -> Bool

neq :: Eq a => a -> a -> Bool
neq a a' = not (eq a a')

class Eq a => Ord a where
  compare :: a -> a -> Ordering

data Ordering :: Type where
  LT :: Ordering
  EQ :: Ordering
  GT :: Ordering

class Ord a => Bounded a where
  minBound :: a
  maxBound :: a

class Semigroup a where
  append :: a -> a -> a

class Semigroup a => Monoid a where
  mempty :: a

class Semiring a where
  add  :: a -> a -> a
  zero :: a
  mul  :: a -> a -> a
  one  :: a

class Ring a where
  sub :: a -> a -> a

class Field a where
  recip :: a -> a

class Heyting a where
  ff :: a
  tt :: a
  implies :: a -> a -> a
  conj    :: a -> a -> a
  disj    :: a -> a -> a
  not     :: a -> a

infixr 3 &&
infixr 2 ||
(&&) :: Heyting a => a -> a -> a
(&&) = conj
(||) :: Heyting a => a -> a -> a
(||) = disj

class Functor f where
  map :: forall a b. (a -> b) -> (f a -> f b)

-- 'Control' classes
class Semigroupoid a where
  compose :: forall b c d. a c d -> a b c -> a b d

class Semigroupoid a => Category a where
  id :: forall t. a t t

class Functor f => Apply f where
  apply :: forall a b. f (a -> b) -> (f a -> f b)

class Apply f => Applicative f where
  pure :: forall a. a -> f a

class Apply m => Monad m where
  bind :: forall a b. m a -> (a -> m b) -> m b

-- Instances
instance Heyting Unit where
  ff = Unit
  tt = Unit
  implies _ _ = Unit
  conj    _ _ = Unit
  disj    _ _ = Unit
  not     _   = Unit

instance Heyting Bool where
  ff = False
  tt = True
  implies a b = not a || b
  conj True True = True
  conj _    _    = False
  disj True _    = True
  disj _    True = True
  disj _    _    = False
  not  True      = False
  not  _         = True

instance Heyting b => Heyting (a -> b) where
  ff _ = tt
  tt _ = tt
  implies f g a = f a `implies` g a
  conj f g a = f a && g a
  disj f g a = f a || g a
  not f a = not (f a)
