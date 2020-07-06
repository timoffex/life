{-# LANGUAGE DeriveFunctor #-}

module RectGrid
  ( RectGrid
  , rowMajorData
  , rowMajorList
  , numRows
  , numCols
  , constant
  , (//)
  , (!)
  , imap
  , unorderedNeighbors
  ) where

import Data.Foldable
import qualified Data.Array as A
import           Data.Maybe

-- | A rectangular grid with zero-based (row, col) coordinates.
data RectGrid a = RectGrid
  { rowMajorData :: A.Array Int a
  , numRows :: Int
  , numCols :: Int
  } deriving ( Functor )

rowMajorList = toList . rowMajorData

constant :: Int -> Int -> a -> RectGrid a
constant n m a
  | n > 0 && m > 0 = RectGrid
                     { rowMajorData = A.array (0, n * m - 1) [(i, a) | i <- [0..n*m-1]]
                     , numRows = n
                     , numCols = m }
  | otherwise      = error $ "Invalid grid size (" ++ show n ++ ", " ++ show m ++ ")"

(//) :: RectGrid a -> [((Int, Int), a)] -> RectGrid a
grid // elts = RectGrid
               { rowMajorData = rowMajorData grid A.// [(toIndex grid (r, c), a)
                                                       | ((r, c), a) <- elts ]
               , numRows = numRows grid
               , numCols = numCols grid }

imap :: ((Int, Int) -> a -> b) -> RectGrid a -> RectGrid b
imap f grid = let rows = numRows grid
                  cols = numCols grid
              in RectGrid
                 { rowMajorData = A.array (0, rows * cols - 1)
                                          [ (i, f (i `div` cols, i `mod` cols) a)
                                          | (i, a) <- zip [0..] $ toList $ rowMajorData grid ]
                 , numRows = rows
                 , numCols = cols }

-- | Gets the value at position (row, col), assuming (row, col) are in range.
(!) :: RectGrid a -> (Int, Int) -> a
(!) grid (r, c) = rowMajorData grid A.! toIndex grid (r, c)

(!?) :: RectGrid a -> (Int, Int) -> Maybe a
(!?) grid (r, c) =
  if r < 0 || r >= numRows grid ||
     c < 0 || c >= numCols grid
    then Nothing
    else Just $ grid ! (r, c)

-- | Gets the row-major zero-based index of the (r, c) position in the grid.
toIndex :: RectGrid a -> (Int, Int) -> Int
toIndex grid (r, c) = r * numCols grid + c

unorderedNeighbors :: RectGrid a -> RectGrid (a, [a])
unorderedNeighbors grid = (`imap` grid) $ \(r, c) a ->
  (a, catMaybes [
                  grid !? (r - 1, c - 1),
                  grid !? (r    , c - 1),
                  grid !? (r + 1, c - 1),
                  grid !? (r - 1, c    ),
                  grid !? (r + 1, c    ),
                  grid !? (r - 1, c + 1),
                  grid !? (r    , c + 1),
                  grid !? (r + 1, c + 1)])
