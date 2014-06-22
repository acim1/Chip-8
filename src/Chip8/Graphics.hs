module Chip8.Graphics
(
) where

import Chip8
import Data.Array.IO
import Data.Bits
import Data.List
import Data.Maybe
import Graphics.Gloss

type Dimensions = ((Y,X),(Y,X))
type PixelArr   = IOArray (Int,Int) Pixel

yLo,xLo,yHi,xHi :: Int
((yLo,xLo),(yHi,xHi)) = ((0,0),(31,63))

displayDimensions :: Dimensions
displayDimensions = ((yLo,xLo),(yHi,xHi))

mkPixelArray :: IO PixelArr
mkPixelArray = newListArray displayDimensions (repeat False)

-- debug
prntArr :: IO PixelArr -> IO ()
prntArr arr = do
    arr'   <- arr
    assocs <- getAssocs arr'
    putStrLn $ show assocs
