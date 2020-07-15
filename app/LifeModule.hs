{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module LifeModule
  ( installLifeGame
  , cellScale
  ) where

import           BaseGame                    (addRenderHook, addUpdateHook)
import           Control.Lens                (over, to, view, (<&>))
import           DeepHas                     (fromNested)
import           Graphics.Gloss.Data.Color   (black)
import           Graphics.Gloss.Data.Picture (color, pictures, rectangleSolid,
                                              rectangleWire, scale, translate)
import           InfiniteGrid                (assocs, emptyGrid,
                                              unorderedNeighbors, (//))
import           World                       (addComponent)


-- Well holy crap. This type signature allows GHC to infer the type!
-- It can't infer the constraint correctly without it, but wow!!!!!
installLifeGame :: _ => _ -> m a
installLifeGame
  = addComponent initialWorld
  . addRenderHook (view $ fromNested . to drawGrid)
  . addUpdateHook (\dt -> over fromNested updateGrid)

initialWorld = emptyGrid // [ ((r, c), False)
                            | r <- [-20 .. 20]
                            , c <- [-20 .. 20] ]

isFilled = id
filledRectangle = color black $ rectangleSolid 1 1
emptyRectangle = color black $ rectangleWire 1 1
cellScale = 20

drawGrid grid =
  scale cellScale cellScale $
    pictures (assocs grid <&>
      \((r, c), cell) ->
        translate (fromIntegral c) (negate $ fromIntegral r) $
        if isFilled cell
          then filledRectangle
          else emptyRectangle)


updateGrid = over id $ fmap updateLifeCell . unorderedNeighbors
updateLifeCell (cell, nghbrs) =
  let total = count nghbrs
      count []         = 0
      count (True:bs)  = 1 + count bs
      count (False:bs) = count bs
  in if not cell
       then total == 3
       else total == 2 || total == 3
