{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MonoLocalBinds         #-}
{-# LANGUAGE PartialTypeSignatures  #-}
{-# LANGUAGE TemplateHaskell        #-}

module MouseModule
  ( installMouseModule
  , MouseState
  , HasClicked (..)
  , HasPosition (..)
  , HasLastPosition (..)
  , HasIsLeftDown (..)
  , HasIsRightDown (..)
  ) where

import           BaseGame                           (addEventHook,
                                                     addUpdateHook)
import           Control.Lens                       (has, makeFields, over, use,
                                                     (&), (.=), (^?))
import           Control.Monad.Extra                (ifM, whenJust)
import           Control.Monad.State                (execState, when)
import           DeepHas                            (fromNested)
import           GlossLenses                        (_EventKey, _MouseButton,
                                                     _MouseLeftButton, _Up)
import           Graphics.Gloss.Interface.Pure.Game (Event (..), KeyState (..),
                                                     MouseButton (..))
import           LensHelpers                        (aside1, aside2)
import           World                              (addComponent)

data MouseState = MouseState
  { _mouseStateClicked      :: Bool
  , _mouseStateWasClicked   :: Bool
  , _mouseStateIsLeftDown   :: Bool
  , _mouseStateIsRightDown  :: Bool
  , _mouseStatePosition     :: (Float, Float)
  , _mouseStateLastPosition :: (Float, Float) }

$( makeFields ''MouseState )

installMouseModule :: _ => _ -> m a
installMouseModule
  = addComponent (MouseState False False False False (0, 0) (0, 0))
  . addEventHook (over fromNested . parseEvent)
  . addUpdateHook (const $ over fromNested updateMouse)

parseEvent :: Event -> MouseState -> MouseState
parseEvent evt = execState $ do
  whenJust (case evt of
              EventMotion mp    -> Just mp
              EventKey _ _ _ mp -> Just mp
              _                 -> Nothing) $ \p -> do
    pos <- use position
    lastPosition .= pos
    position     .= p


  whenJust (evt ^? (_EventKey . aside1 _MouseButton)) $ \(b, ks, _, _) ->
    case b of
      LeftButton  -> isLeftDown  .= (ks == Down)
      RightButton -> isRightDown .= (ks == Down)
      _           -> return ()

  when (evt & has (_EventKey . aside1 _MouseLeftButton . aside2 _Up)) $ do
    clicked    .= True
    wasClicked .= True

updateMouse :: MouseState -> MouseState
updateMouse = execState $
  ifM (use wasClicked)
    ( wasClicked .= False )
    ( clicked .= False )
