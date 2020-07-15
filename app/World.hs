{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module World
  ( World (World)
  , AddComponentC
  , addComponent
  , OutermostWorld
  , WorldType
  , OutermostWorldType
  ) where

import Control.Algebra ( Algebra (..), Has )
import Control.Effect.Sum ( (:+:) (..) )
import Control.Effect.Reader ( Reader (..), ask, local )


-- | A tag (aka an annotation) for a type representing the state of
-- the game.
newtype World w = World w

-- | A carrier for @'Reader' ('World' (s, w))@.
--
-- This uses a lower-level carrier to get @'World' w@.
newtype AddComponentC s w m a = AddComponentC (s -> m a) deriving Functor

-- | Adds some state to the 'World'.
addComponent s (AddComponentC f) = f s


-- | Extracts the type of the outermost 'Reader' 'World'.
--
-- For example,
--
-- @
--   OutermostWorld (Reader (World a) :+: Reader (World b)) = World a
-- @
--
-- This is meant to be used in the type applications for 'ask' when
-- used with 'addComponent' to avoid having to know the final 'World'
-- type:
--
-- @
--   runReader (World ())    -- Start with an empty world.
--     . addComponent 0        -- Add a (Num a => a) component.
--     $ playGame
--   where
--     playGame :: Has (Reader (OutermostWorld sig)) sig m => m ()
--     playGame = do
--       w <- ask @(OutermostWorld sig)
--       -- do something with w
-- @
--
-- Use 'OutermostWorldType' to put constraints on the @w@ in @World
-- w@.
type family OutermostWorld sig where
  OutermostWorld (Reader (World w))          = World w
  OutermostWorld (Reader (World w) :+: rest) = World w
  OutermostWorld (_ :+: rest)                = OutermostWorld rest

type family WorldType t where
  WorldType (World w) = w

type OutermostWorldType sig = WorldType (OutermostWorld sig)



instance Applicative m => Applicative (AddComponentC s w m) where
  pure = AddComponentC . const . pure
  a1 <*> a2 = AddComponentC $ \s -> addComponent s a1 <*> addComponent s a2

instance Monad m => Monad (AddComponentC s w m) where
  a >>= f = AddComponentC $ \s -> addComponent s a >>= (addComponent s . f)

instance ( w ~ OutermostWorldType sig
         , Has (Reader (World w)) sig m
         , Algebra sig m
         ) => Algebra (Reader (World (s, w)) :+: sig)
                      (AddComponentC s w m) where
  alg hdl sig ctx = AddComponentC $ \s -> case sig of
    L Ask -> do
      -- Read w from the lower-level carrier and return (s, w).
      World w <- ask
      return $ World (s, w) <$ ctx
    L (Local f n) -> do
      -- Read w from the lower-level carrier and run the given
      -- computation, using s' instead of s and w' instead of w.
      World w <- ask
      let World (s', w') = f $ World (s, w)
          this_ctx_a = hdl $ n <$ ctx
      local (const $ World w') $ addComponent s' this_ctx_a
    R other ->
      -- Not an effect that we handle; pass to a lower-level carrier.
      alg (addComponent s . hdl) other ctx
