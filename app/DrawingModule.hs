{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeApplications      #-}

module DrawingModule
  ( installDrawingModule
  ) where

import           BaseGame            (addEventHook)
import           Control.Lens        (use, (%=))
import           Control.Monad.Extra (whenM)
import           Control.Monad.State (execState)
import           DeepHas             (DeepHas, fromNested, liftNested)
import           InfiniteGrid        (InfiniteGrid, (//))
import           LifeModule          (cellScale)
import           MouseModule         (MouseState, isLeftDown, position)
import           PanningModule       (PanState, translation)

installDrawingModule :: _ => _ -> m a
installDrawingModule = addEventHook (const paintNewCells)

paintNewCells :: ( DeepHas MouseState w
                 , DeepHas PanState w
                 , DeepHas (InfiniteGrid Bool) w
                 ) => w -> w
paintNewCells = execState $
  whenM (use $ liftNested (isLeftDown @MouseState)) $ do
    (mx, my) <- use (liftNested $ position @MouseState)
    (tx, ty) <- use (liftNested $ translation @PanState)
    let (r, c) = ( round $ (ty - my) / cellScale
                 , round $ (mx - tx) / cellScale )
        newCell = True

    fromNested %= (// [((r, c), newCell)])
