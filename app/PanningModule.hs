{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MonoLocalBinds         #-}
{-# LANGUAGE PartialTypeSignatures  #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeApplications       #-}

module PanningModule
  ( installPanningModule
  , PanState
  , HasTranslation (..)
  ) where

import           BaseGame                    (addEventHook, addPostRenderHook)
import           Control.Lens                (makeFields, over, use, view, (%=),
                                              (^.))
import           Control.Monad.Extra         (whenM)
import           Control.Monad.State         (execState)
import           DeepHas                     (DeepHas, fromNested, liftNested)
import           Graphics.Gloss.Data.Picture (Picture, translate)
import           MouseModule                 (MouseState, isRightDown,
                                              lastPosition, position)
import           World                       (addComponent)

data PanState = PanState
  { _panStateIsPanning   :: Bool
  , _panStateTranslation :: (Float, Float)
  }
$( makeFields 'PanState )

installPanningModule :: _ => _ -> m a
installPanningModule
  = addComponent (PanState False (0, 0))
  . addEventHook (const $ over id update)
  . addPostRenderHook (panPicture . view fromNested)


update :: ( DeepHas PanState w
          , DeepHas MouseState w )
          => w -> w
update = execState $
  whenM (use $ liftNested (isRightDown @MouseState)) $ do
    (x1, y1) <- use $ liftNested (lastPosition @MouseState)
    (x2, y2) <- use $ liftNested (position @MouseState)
    liftNested (translation @PanState) %= \(tx, ty) ->
      (tx + x2 - x1, ty + y2 - y1)

panPicture :: PanState -> Picture -> Picture
panPicture panState = let (x, y) = panState ^. translation
                      in translate x y

