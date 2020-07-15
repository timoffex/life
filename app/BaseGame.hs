{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

-- | The base DI module for running a Gloss game.
module BaseGame
  ( installGameBase
  , playGame
  , PostRenderHook (..)
  , addPostRenderHook
  , RenderHook (..)
  , addRenderHook
  , UpdateHook (..)
  , addUpdateHook
  , EventHook (..)
  , addEventHook
  ) where


import           Adjust                             (adjust)
import           Control.Carrier.Reader             (ask, runReader)
import           Control.Effect.Lift                (Has, Lift, sendM)
import           Control.Effect.Reader              (Reader)
import           Control.Effect.Sum                 ((:+:))
import           Graphics.Gloss.Data.Picture        (Picture, blank)
import           Graphics.Gloss.Interface.Pure.Game (Display (InWindow), Event,
                                                     play, white)
import           World                              (OutermostWorld,
                                                     World (World))


newtype PostRenderHook w = PostRenderHook (w -> Picture -> Picture)
newtype RenderHook w = RenderHook (w -> Picture)
newtype UpdateHook w = UpdateHook (Float -> w -> w)
newtype EventHook w = EventHook (Event -> w -> w)


addPostRenderHook h = adjust $ \(PostRenderHook f) -> PostRenderHook $ \w -> h w . f w
addRenderHook h = adjust $ \(RenderHook f) -> RenderHook $ \w -> f w <> h w
addUpdateHook h = adjust $ \(UpdateHook f) -> UpdateHook $ \dt -> h dt . f dt
addEventHook h = adjust $ \(EventHook f) -> EventHook $ \evt -> h evt . f evt


-- | Installs the base hooks and data needed for 'playGame'.
installGameBase :: _ -> m a
installGameBase = runReader (World ())
                . runReader (RenderHook $ const blank)
                . runReader (PostRenderHook $ const id)
                . runReader (UpdateHook $ const id)
                . runReader (EventHook $ const id)


-- DeepHas w1 w, Has (Reader (RenderHook w1)) sig m
--
-- There's an ambiguity problem here. There could be multiple possible
-- w1's in the stack. Clearly, I want the outermost RenderHook w1
-- while also requiring that DeepHas w1 w. This is another "outermost"
-- type family!

type family OutermostReader f sig where
  OutermostReader f (Reader (f a) :+: sig) = f a
  OutermostReader f (Reader (f a)        ) = f a
  OutermostReader f (_ :+: sig)            = OutermostReader f sig


-- | Injects the game configuration and plays the game.
--
-- This injects a 'RenderHook', 'UpdateHook', 'EventHook' and an
-- initial 'World'. To add hooks, use 'Adjust.adjust'. The hooks
-- should be polymorphic such that they can work on the entire world
-- state, so use 'DeepHas.liftNested'. To add state to the 'World',
-- use 'World.addComponent'.
--
-- This is compile-time dependency injection using Haskell's type
-- system & inference. Yes, it's f-ing sick.
playGame :: forall sig m w.
            ( OutermostWorld sig ~ World w
            , OutermostReader RenderHook sig ~ RenderHook w
            , OutermostReader PostRenderHook sig ~ PostRenderHook w
            , OutermostReader UpdateHook sig ~ UpdateHook w
            , OutermostReader EventHook sig ~ EventHook w
            , Has (Reader (World w)) sig m
            , Has (Reader (RenderHook w)) sig m
            , Has (Reader (PostRenderHook w)) sig m
            , Has (Reader (UpdateHook w)) sig m
            , Has (Reader (EventHook w)) sig m
            , Has (Lift IO) sig m
            ) => m ()
playGame = do
  World initialWorld <- ask @(World w)
  RenderHook renderHook <- ask @(RenderHook w)
  PostRenderHook postRenderHook <- ask @(PostRenderHook w)
  UpdateHook updateHook <- ask @(UpdateHook w)
  EventHook eventHook <- ask @(EventHook w)

  sendM $
    play (InWindow "Conway's Game of Life" (800, 600) (0, 0))
      white
      2
      initialWorld
      (\w -> postRenderHook w $ renderHook w)
      eventHook
      updateHook

