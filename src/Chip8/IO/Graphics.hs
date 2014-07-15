module Chip8.IO.Graphics where

import Chip8
import Control.Monad
import Data.Array.IO
import Data.Bits
import Data.List
import Data.Maybe
import Graphics.Gloss

-------------------------------------------------------------------------------
-- Primary Types and Functions
-------------------------------------------------------------------------------

type Dimensions = ((Y,X),(Y,X))
type PixelArray   = IOArray (Int,Int) Pixel

yLo,xLo,yHi,xHi :: Int
((yLo,xLo),(yHi,xHi)) = ((0,0),(31,63))

displayDimensions :: Dimensions
displayDimensions = ((yLo,xLo),(yHi,xHi))

mkPixelArray :: IO PixelArray
mkPixelArray = newListArray displayDimensions (repeat False)

-- | Writes sprite onto pixel array, and returns whether or not a collision
--   occured. Writing occurs in a vertical fashion, writing 8 pixels horizontally
--   and then moving down a row. 
writeSprite :: PixelArray -> Sprite -> IO Bool
writeSprite arr ((x,y),pxString) =
    foldM (\b (pxByte,i) -> do
            b' <- writePixelByte arr (x, wrapy (y+i)) pxByte
            return $ b' || b
          )
    False $ zip pxString $ [0..(length pxString)-1]

writePixelByte :: PixelArray -> (X,Y) -> PixelByte -> IO Bool
writePixelByte arr (x,y) pxs = 
    foldM (\b (px,i) -> do
            b' <- writePixelBit arr (wrapx (x+i),y) px
            return $ b' || b
          ) 
    False $ zip pxs [0..7] 

-- | Writes as (y,x) or reversed, as y represents the row
writePixelBit :: PixelArray -> (X,Y) -> Pixel -> IO Bool
writePixelBit arr (x,y) newpx = do
    oldpx <- readArray arr (y,x)
    let px = bxor newpx oldpx
    writeArray arr (y,x) px
    return $ oldpx && newpx -- collision

-------------------------------------------------------------------------------
-- Helper Functions
-------------------------------------------------------------------------------

bxor :: Bool -> Bool -> Bool
bxor = (/=)    

wrapx :: X -> X
wrapx x = x `mod` 64

wrapy :: Y -> Y
wrapy y = y `mod` 32

pixelPicture :: PixelArray -> IO Picture
pixelPicture arr = liftM pictures $
    foldM (\pxs (x,y) -> do
               b <- readArray arr (y,x)
               if b
                   then return $ (pixel (x,y)) : pxs
                   else return pxs
          )
    [] [(x,y) | y <- [0..31], x <- [0..63]]

     

-- | Places one 10 x 10 pixel, centered about the origin to a location on the 64 x 32 (640 x 320) Chip8 display
pixel :: (X,Y) -> Picture
pixel (x,y)  = 
    translate (float $ adjust x 32) (float $ negate (adjust y 16)) newPixel
  where
    float = fromIntegral
    adjust q r 
        | q < r = (-r * 10) + (q * 10) + 5     
        | otherwise = (q * 10) - (r * 10) + 5

-- | Makes one 10 x 10 green pixel, centered about the origin
newPixel :: Picture
newPixel = color green $ rectangleSolid 10 10

-------------------------------------------------------------------------------
-- Debug
-------------------------------------------------------------------------------

prntArr :: PixelArray -> IO ()
prntArr arr = do
    assocs <- getAssocs arr
    putStrLn $ show assocs


showDisplay :: PixelArray -> IO ()
showDisplay arr = do
    picture <- pixelPicture arr
    display (InWindow "Chip-8" (640,320) (0,0)) black picture


-------------------------------------------------------------------------------
-- Commented Out Example Code
-------------------------------------------------------------------------------

-- pictures [plotPixel (63,31) pixel, plotPixel (0,0) pixel]

-- how to put a 10 x 10 pixel in the upper corner
-- color green $ translate (-315) 155 $ rectangleSolid 10 10

-- main = (newListArray displayDimensions (cycle [True,True,False])) >>= showDisplay

  
