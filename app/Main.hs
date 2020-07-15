module Main where

import           BaseGame             (installGameBase, playGame)
import           Control.Carrier.Lift (runM)
import           DrawingModule        (installDrawingModule)
import           LifeModule           (installLifeGame)
import           MouseModule          (installMouseModule)
import           PanningModule        (installPanningModule)

main :: IO ()
main = runM
     . installGameBase
     . installMouseModule
     . installPanningModule
     . installLifeGame
     . installDrawingModule
     $ playGame
