{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import InfiniteGrid
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Data.ViewPort
import Control.Lens hiding (imap)
import Control.Monad.State.Strict
import Control.Monad.Extra
import Data.Functor
import Data.Maybe
import Data.Proxy
import Data.Monoid

type Cell = Bool

$( makePrisms ''Event )
$( makePrisms ''Key )
$( makePrisms ''MouseButton )
$( makePrisms ''SpecialKey )
$( makePrisms ''KeyState )

_MouseRightButton :: Prism' Key ()
_MouseRightButton = _MouseButton._RightButton

data MouseState = MouseState
  { _mouseStatePosition  :: (Float, Float)
  , _mouseStateIsDown    :: Bool
  , _mouseStateClicked   :: Bool
  , _mouseStateModifiers :: Modifiers
  }
$( makeFields 'MouseState )

data PanState = PanState
  { _panStateLastMousePosition :: (Float, Float)
  , _panStateIsPanning :: Bool
  }
$( makeFields 'PanState )

data World = World
  { _cells :: InfiniteGrid Cell
  , _viewPort :: ViewPort
  , _mouse :: MouseState
  , _panState :: PanState
  , _isRunning :: Bool }
$( makeLenses 'World )

translation :: Lens' ViewPort (Float, Float)
translation = lens viewPortTranslate $ \vp t -> vp { viewPortTranslate = t }

main :: IO ()
main = playIO
         (InWindow "Conway's Game of Life" (800, 600) (0, 0))
         white   -- background color
         2       -- # simulation steps per second
         initialWorld
         drawWorld
         processEvent
         updateWorld

cellScale = 20
drawGrid grid = return $
  scale cellScale cellScale $
    pictures (assocs grid <&>
      \((r, c), cell) ->
        translate (fromIntegral c) (negate $ fromIntegral r) $
        if isFilled cell
          then filledRectangle
          else emptyRectangle)

drawWorld world =
  let (x, y) = world^.viewPort.translation
  in translate x y <$> drawGrid (world^.cells)

updateWorld dt = execStateT $
  whenM (use isRunning) $
    cells %= fmap updateLifeCell . unorderedNeighbors

updateLifeCell (cell, nghbrs) =
  let total = count nghbrs
      count [] = 0
      count (True:bs) = 1 + count bs
      count (False:bs) = count bs
  in if not cell
       then total == 3
       else total == 2 || total == 3

initialPanState = PanState
  { _panStateIsPanning = False
  , _panStateLastMousePosition = (0, 0) }

initialWorld = World { _cells = emptyGrid // [ ((r, c), False)
                                             | r <- [-20 .. 20]
                                             , c <- [-20 .. 20] ]
                     , _viewPort = ViewPort
                         { viewPortTranslate = (0, 0)
                         , viewPortRotate = 0
                         , viewPortScale = 0 }
                     , _mouse = MouseState
                         { _mouseStateIsDown    = False
                         , _mouseStatePosition  = (0, 0)
                         , _mouseStateClicked   = False
                         , _mouseStateModifiers = Modifiers Up Up Up }
                     , _panState = initialPanState
                     , _isRunning = False }

isFilled cell = cell
filledRectangle = color black $ rectangleSolid 1 1
emptyRectangle = color black $ rectangleWire 1 1

processEvent :: Event -> World -> IO World
processEvent evt world = (`execStateT` world) $ do
  updateMouseState evt
  updatePanning evt
  processKeys evt
  paintNewCells

-- | Applies a prism to the first element of a tuple of any length.
--
-- @
--   (Just 1, "a") ^? aside1 _Just        ==  Just (1, "a")
--   (Nothing, 3, 8, 9) ^? aside1 _Just   ==  Nothing
--   aside1 _Just # (1, "a")              == (Just 1, "a")
-- @
aside1 :: (Field1 s' a' s a, Field1 a' s' a s, Field1 s' s' s s) => Prism' s a -> Prism' s' a'
aside1 p = prism' (over _1 $ review p) (\s' -> do
                                          a <- s' ^? _1 . p
                                          return $ s' & _1 .~ a)

-- | Applies a prism to the second element of a tuple of any length.
--
-- See 'aside1' documentation for examples.
aside2 :: (Field2 s' a' s a, Field2 a' s' a s, Field2 s' s' s s) => Prism' s a -> Prism' s' a'
aside2 p = prism' (over _2 $ review p) (\s' -> do
                                          a <- s' ^? _2 . p
                                          return $ s' & _2 .~ a)

-- | Modifies the target of a traversal while within a MonadState using a StateT.
--
-- @
--   printAndFlipMouse :: StateT World IO ()
--   printAndFlipMouse = do
--     mouse %=% do
--       oldPosition <- use position
--       liftIO $ print $ "Old position: " ++ show oldPosition
--       position .= (oldPosition & both %~ negate)
-- @
--
--p This composes well:
--
-- @
--   printAndFlipMice :: StateT Multiverse IO ()
--   printAndFlipMice =
--     traverseWorlds %=% printAndFlipMouse
-- @
(%=%) :: (MonadState s m) => Traversal s s a a -> StateT a m () -> m ()
infix 4 %=%
tr %=% m = do
  s  <- get
  s' <- s & tr %%~ execStateT m
  put s'

-- | Combines '(%=)' and '(//)' to update an 'InfiniteGrid' target of a setter.
(%=//) :: (MonadState s m)
       => ASetter' s (InfiniteGrid a)
       -> [((Int, Int), a)]
       -> m ()
infix 4 %=//
grid %=// updates = grid %= (// updates)

processKeys evt =
  when (evt & has (_EventKey . aside1 (_SpecialKey . _KeySpace) . aside2 _Up)) $
    isRunning %= not

paintNewCells =
  whenM (use $ mouse.isDown) $ do
    (r, c)  <- uses id $ \w -> toRowCol w (w^.mouse.position)
    newCell <- uses id $ has $ mouse.modifiers.to shift._Up

    cells %=// [((r, c), newCell)]

toRowCol world (x, y) = (r, c)
  where
    (wx, wy) = world^.viewPort.translation
    c = round $ (x - wx) / cellScale
    r = round $ (wy - y) / cellScale

updateMouseState evt = do
  whenJust (evt ^? _EventMotion) $ \mp ->
    mouse %=% do
      position .= mp
      clicked  .= False

  whenJust (evt ^? _EventKey . aside1 _MouseButton) $ \(b, ks, mods, mp) -> do
    mouse %=% do
      position  .= mp
      modifiers .= mods
    if b & isn't _LeftButton
      then mouse %= set clicked False
      else mouse %=% do
             wasDown <- use isDown
             clicked .= (wasDown && ks == Up)
             isDown  .= (ks == Down)

updatePanning evt =
  ifM (use $ panState.isPanning)
    -- then
    ( do
        (px, py) <- use $ panState.lastMousePosition
        (nx, ny) <- use $ mouse.position
        let (dx, dy) = (nx - px, ny - py)
        panState %=% do
          lastMousePosition .= (nx, ny)
          isPanning         .= (evt & isn't (_EventKey.aside1 _MouseRightButton.aside2 _Up))
        viewPort.translation %= \(x, y) -> (x + dx, y + dy) )
    -- else
    ( when (evt & has (_EventKey.aside1 _MouseRightButton.aside2 _Down)) $ do
        world <- get
        panState %=% do
          lastMousePosition .= world^.mouse.position
          isPanning         .= True )
