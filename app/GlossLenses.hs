{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

module GlossLenses
  ( _EventKey
  , _EventMotion
  , _MouseButton
  , _MouseRightButton
  , _MouseLeftButton
  , _RightButton
  , _LeftButton
  , _SpecialKey
  , _KeySpace
  , _Down
  , _Up
  ) where

import Control.Lens
import Graphics.Gloss.Interface.Pure.Game

$( makePrisms ''Event )
$( makePrisms ''Key )
$( makePrisms ''MouseButton )
$( makePrisms ''SpecialKey )
$( makePrisms ''KeyState )

_MouseRightButton :: Prism' Key ()
_MouseRightButton = _MouseButton._RightButton

_MouseLeftButton :: Prism' Key ()
_MouseLeftButton = _MouseButton._LeftButton
