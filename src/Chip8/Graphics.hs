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
wrapx x = x `mod` 64

wrapy :: Y -> Y
wrapy y = y `mod` 32

-- debug
prntArr :: PixelArr -> IO ()
prntArr arr = do
    assocs <- getAssocs arr
    putStrLn $ show assocs


showDisplay :: PixelArr -> IO ()
showDisplay arr = animate (InWindow "Chip-8" (640,320) (0,0))
    black (pixelPicture arr)

-- TODO: Flesh out to render array...
pixelPicture :: PixelArr -> Float -> Picture
pixelPicture arr t = plotPixel (63,31) mkPixel

-- | "Moves" one 10 x 10 pixel, centered about the origin to a location
--   on the 64 x 32 Chip8 display
plotPixel :: (X,Y) -> Picture -> Picture
plotPixel (x,y) px = 
    translate (float $ adjust x 32) (float $ negate (adjust y 16)) px
  where
    float = fromIntegral
    adjust q r 
        | q < r = (-r * 10) + (q * 10) + 5     
        | otherwise = (q * 10) - (r * 10) + 5

-- | Makes one 10 x 10 green pixel, centered about the origin
mkPixel :: Picture
mkPixel = color green $ rectangleSolid 10 10

-- how to put a 10 x 10 pixel in the upper corner
-- color green $ translate (-315) 155 $ rectangleSolid 10 10

--debug
main = mkPixelArray >>= showDisplay
    
    
