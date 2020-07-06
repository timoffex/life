module RectGridSpec (spec) where

import RectGrid
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Control.Monad

spec = do
  describe "args to constant" $ do
    it "are numRows and then numCols" $ property $
      \nr nc -> let nr' = getPositive nr
                    nc' = getPositive nc
                    grid = constant nr' nc' 0
                in (nr', nc') == (numRows grid, numCols grid)
                   
  describe "imap" $ do
    it "uses 0-based (row, col) indices" $ do
      let rowData = [ ((0, 0), "00")
                    , ((0, 1), "01")
                    , ((0, 2), "02")
                    , ((1, 0), "10")
                    , ((1, 1), "11")
                    , ((1, 2), "12") ]
          grid = constant 2 3 "x" // rowData
      (rowMajorList $ imap (,) grid) `shouldBe` rowData

  describe "(!)" $ do
    it "inverts (//)" $ do
      let zeros = constant 5 3 0
      forM_ [0..4] $ \r ->
        forM_ [0..2] $ \c ->
          (zeros // [((r, c), 1)]) ! (r, c) `shouldBe` 1
      
