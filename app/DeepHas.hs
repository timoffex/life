{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Defines the 'liftNested' combinator that lifts a lens-like to a
-- nested pair.
--
-- For example, if you have a @Lens' S T@, then you can get a @Lens'
-- (X, (Y, (S, Z))) T@ by applying 'liftNested'.
module DeepHas (DeepHas, liftNested, fromNested) where

import GHC.TypeNats (Nat, KnownNat, type (+), type (<=), type (-))
import Control.Lens (_1, _2, LensLike)
import Data.Proxy (Proxy (Proxy))
import Data.Type.Equality (type (==))

class DeepHas t s where
  -- | Lifts a lens-like to a right-nested pair.
  --
  -- If you have a @Lens' S T@, then you can get a @Lens' (X, (Y, (S,
  -- Z))) T@ by applying this, assuming that @X, Y, S, Z@ are all
  -- distinct types.
  --
  -- NOTE: This doesn't work well with @makeFields@ from the lens
  -- package. This is because @makeFields@ generates a typeclass, and
  -- typeclasses are open: a programmer can always implement the field
  -- on another type in your nested pair, and then it will be
  -- impossible to figure out which element to apply the lens to. The
  -- cleanest way to solve this is to use a single type application:
  --
  -- @
  --   set (liftNested (myField @MyType)) newValue myNestedType
  -- @
  --
  -- which works because of the lucky order of the type variables on
  -- the generated bindings.
  liftNested :: Functor f
             => LensLike f t t a a
             -> LensLike f s s a a


fromNested :: (Functor f, DeepHas t s) => LensLike f s s t t
fromNested = liftNested id


instance (n ~ IndexOf t s, DeepHas' n t s) => DeepHas t s where
  liftNested = liftNested'

class DeepHas' (index :: Nat) t s | s t -> index, s index -> t where
  liftNested' :: Functor f
              => LensLike f t t a a
              -> LensLike f s s a a

type family IndexOf a s where
  IndexOf a a      = 0
  IndexOf a (a, t) = 1
  IndexOf a (b, t) = 2 + IndexOf a t

type family AtIndex n s where
  AtIndex 0 s      = s
  AtIndex 1 (s, t) = s
  AtIndex n (x, t) = AtIndex (n - 2) t

instance DeepHas' 0 s s where
  liftNested' l = l

-- The OVERLAPPING and OVERLAPPABLE annotations are needed for `stack
-- run` but not `stack build`. I don't know why, and it's too much
-- work to create a minimal reproducible example. `stack run` works
-- fine if I `stack build` first, for obvious reasons.
instance {-# OVERLAPPING #-} DeepHas' 1 s (s, t) where
  liftNested' l = _1 . l

instance {-# OVERLAPPABLE #-}
  ( m ~ IndexOf s (x, t)
  , s ~ AtIndex m (x, t)
  , 2 <= m
  , KnownNat m -- Required for GHC to understand that "2 <= m" prevents
               -- overlapping instances.
  , DeepHas' (IndexOf s t) s t) => DeepHas' m s (x, t) where
  
  liftNested' l = _2 . liftNested l

