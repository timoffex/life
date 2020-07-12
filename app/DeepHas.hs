{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Defines the 'liftNested' combinator that lifts a lens-like to a
-- nested pair.
--
-- For example, if you have a @Lens' S T@, then you can get a @Lens'
-- (X, (Y, (S, Z))) T@ by applying 'liftNested'.
module DeepHas (DeepHas (liftNested)) where

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

class DeepHas' (firstMatches :: Bool) t s where
  liftNested' :: Functor f
              => Proxy firstMatches
              -> LensLike f t t a a
              -> LensLike f s s a a

-- A little trick to avoid GHC from complaining about overlapping
-- instances.
--
-- https://kseo.github.io/posts/2017-02-05-avoid-overlapping-instances-with-closed-type-families.html
type family FirstMatches a s where
  FirstMatches a (a, s) = 'True
  FirstMatches _ _      = 'False

instance DeepHas s s where
  liftNested = id

instance (DeepHas' (FirstMatches t s) t s) => DeepHas t s where
  liftNested = liftNested' $ Proxy @(FirstMatches t s)

instance ((u == x) ~ 'True, u ~ x) => DeepHas' 'True u (x, t) where
  liftNested' _ l = _1 . l

instance ((u == x) ~ 'False, DeepHas u t) => DeepHas' 'False u (x, t) where
  liftNested' _ l = _2 . liftNested l
