{-# LANGUAGE DeriveFunctor #-}

module InfiniteGrid
  ( InfiniteGrid
  , emptyGrid
  , (!?)
  , (//)
  , imap
  , unorderedNeighbors
  , elems
  , assocs
  ) where

import qualified Data.Map as M
import Data.Maybe

newtype InfiniteGrid a = InfiniteGrid
                         { mapData :: M.Map (Int, Int) a
                         } deriving ( Functor )

emptyGrid :: InfiniteGrid a
emptyGrid = InfiniteGrid M.empty

(!?) :: InfiniteGrid a -> (Int, Int) -> Maybe a
(!?) grid (r, c) = M.lookup (r, c) $ mapData grid

(//) :: InfiniteGrid a -> [((Int, Int), a)] -> InfiniteGrid a
grid // [] = grid
grid // ((p, a):rest) = InfiniteGrid (M.insert p a $ mapData grid) // rest

mapWithKey :: ((Int, Int) -> a -> b) -> InfiniteGrid a -> InfiniteGrid b
mapWithKey f = InfiniteGrid . M.mapWithKey f . mapData

imap = mapWithKey

elems = M.elems . mapData
assocs = M.assocs . mapData

unorderedNeighbors :: InfiniteGrid a -> InfiniteGrid (a, [a])
unorderedNeighbors grid = (`mapWithKey` grid) $ \(r, c) a ->
  (a, catMaybes [
                  grid !? (r - 1, c - 1),
                  grid !? (r    , c - 1),
                  grid !? (r + 1, c - 1),
                  grid !? (r - 1, c    ),
                  grid !? (r + 1, c    ),
                  grid !? (r - 1, c + 1),
                  grid !? (r    , c + 1),
                  grid !? (r + 1, c + 1)])
