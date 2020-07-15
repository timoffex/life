{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Adjust
  ( AdjustC
  , adjust
  ) where

import Control.Algebra ( Algebra (..), Has )
import Control.Effect.Reader ( Reader (..), ask, local )
import Control.Effect.Sum ( (:+:) (..) )


-- | Adjusts the output for @'Reader' (tag r)@ effect carriers from lower in
-- the stack.
newtype AdjustC f r m a = AdjustC ((f r -> f r) -> m a) deriving Functor

-- | Adjusts the value read by a @'Reader' (tag r)@ effect.
adjust :: (f r -> f r) -> AdjustC f r m a -> m a
adjust adjustment (AdjustC f) = f adjustment


type family OutermostReader f sig where
  OutermostReader f (Reader (f a) :+: sig) = f a
  OutermostReader f (Reader (f a)        ) = f a
  OutermostReader f (_ :+: sig)            = OutermostReader f sig


instance Applicative m => Applicative (AdjustC f r m) where
  pure = AdjustC . const . pure
  a1 <*> a2 = AdjustC $ \rr -> adjust rr a1 <*> adjust rr a2

instance Monad m => Monad (AdjustC f r m) where
  a >>= f = AdjustC $ \rr -> adjust rr a >>= (adjust rr . f)

instance ( Has (Reader (f r)) sig m
         , f r ~ OutermostReader f sig
         , Algebra sig m
         ) => Algebra (Reader (f r) :+: sig) (AdjustC f r m) where
  alg hdl sig ctx = AdjustC $ \rr -> case sig of
    L Ask -> (<$ ctx) . rr <$> ask
    L (Local f n) -> local f $ adjust (f . rr) (hdl $ n <$ ctx)
    R other -> alg (adjust rr . hdl) other ctx
