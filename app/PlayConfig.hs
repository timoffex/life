{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

module PlayConfig
  ( PlayConfig
  , emptyPlayConfig
  ) where

import Control.Lens
import Graphics.Gloss.Interface.IO.Game

data PlayConfig w = PlayConfig
  { _playConfigInitialWorld   :: w
  , _playConfigSimulateAction :: Float -> w -> IO w
  , _playConfigEventAction    :: Event -> w -> IO w
  , _playConfigRenderAction   :: w -> IO Picture
  }

$( makeFields ''PlayConfig )

-- | A stateless 'PlayConfig' that does nothing.
emptyPlayConfig :: PlayConfig ()
emptyPlayConfig = PlayConfig
  ()
  (\_ _ -> return ())
  (\_ _ -> return ())
  (\_ -> return blank)
