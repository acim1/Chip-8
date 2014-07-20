module Chip8.IO.Run where

import Chip8
import Chip8.IO.Graphics
import qualified Data.ByteString as BS
import qualified Graphics.Gloss as G
import System.Process
import System.Random
import Control.Monad

data World = World {
    chip8         :: Chip8,
    display       :: G.Picture,
    pixels        :: PixelArray,
    cycleRate     :: CPS,   -- cycles per second
    refreshMod    :: Int,   -- (cycleRate / 60 hz)
    refreshCtr    :: Int    -- (running cycle count) mod (refreshMod)  
}

data Parameters = Parms FilePath CPS

type CPS = Int

run :: IO ()
run = do
    w <- initialize
    putStrLn "Loaded."

-------------------------------------------------------------------------------
-- Effects
-------------------------------------------------------------------------------

showDisplay :: World -> IO G.Picture
showDisplay w = return $ display w 

-- when (st > 0) soundEffect
soundEffect :: IO ()
soundEffect = do
    runCommand cmd
    return ()
  where cmd = "aplay -q /home/mike/Downloads/Software/Chip8/pong.wav"


-------------------------------------------------------------------------------  
-- Initialization 
-------------------------------------------------------------------------------
initialize :: IO World
initialize = do
    (Parms fp cps) <- getParameters
    program    <- readData fp
    g          <- getStdGen
    pxs        <- mkPixelArray
    let c8 = loadData (mkChip8 g) 0x200 program
    return $ World c8 G.Blank pxs cps (cps `quot` 60) 0
  where    
    readData fp = do
        contents <- BS.readFile fp
        return $ BS.unpack contents
   

getParameters :: IO Parameters
getParameters = do
    putStr "Chip-8 program filepath: "
    fp      <- getLine
    putStr "Cycles per second (0 for default): "
    cps    <- getLine
    return $ Parms fp (vetCycles $ read cps) 
 where
    vetCycles 0 = 500 -- default
    vetCycles x 
        | x < 60 = 60
        | otherwise = x   
