module Chip8.Graphics
(
) where

import Chip8
import Control.Monad
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

-- | Writes sprite onto pixel array, and returns whether or not a collision
--   occured. Writing occurs in a vertical fashion, writing 8 pixels horizontally
--   and then moving down a row. 
writeSprite :: PixelArr -> Sprite -> IO Bool
writeSprite arr ((x,y),pxString) =
    foldM (\b (pxByte,i) -> do
            b' <- writePixelByte arr (x, wrapy (y+i)) pxByte
            return $ b' || b
          )
    False $ zip pxString $ [0..(length pxString)-1]

writePixelByte :: PixelArr -> (X,Y) -> PixelByte -> IO Bool
writePixelByte arr (x,y) pxs = 
    foldM (\b (px,i) -> do
            b' <- writePixelBit arr (wrapx (x+i),y) px
            return $ b' || b
          ) 
    False $ zip pxs [0..7] 

-- | Writes as (y,x) or reversed, as y represents the row
writePixelBit :: PixelArr -> (X,Y) -> Pixel -> IO Bool
writePixelBit arr (x,y) b = do
    b' <- readArray arr (y,x)
    let collision = b' && not b
    writeArray arr (y,x) b
    return collision
    

wrapx :: X -> X
wrapx x = x `mod` 32

wrapy :: Y -> Y
wrapy y = y `mod` 64

-- debug
prntArr :: PixelArr -> IO ()
prntArr arr = do
    assocs <- getAssocs arr
    putStrLn $ show assocs
