module Chip8.IO.Run where

import Chip8
import Chip8.IO.Graphics
import Chip8.Processing
import Control.Applicative
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import qualified Data.ByteString as BS
import qualified Graphics.Gloss as G
import System.Process
import System.Random
import Control.Monad

data World = World {
    chip8         :: Chip8,
    display       :: G.Picture,
    pxArray       :: PixelArray,
    cycleRate     :: CPS,   -- cycles per second
    refreshMod    :: Int,   -- (cycleRate / 60 hz)
    refreshCtr    :: Int    -- (running cycle count) mod (refreshMod)  
}

data Parameters = Parms FilePath CPS
    deriving Show

type CPS = Int

run :: IO ()
run = do
    w <- initialize
    putStrLn "Loaded."

-------------------------------------------------------------------------------
-- Step World
-------------------------------------------------------------------------------

step' :: StateT World IO ()
step' = do
    wDisplayUpd
    modify wRefreshUpd
    wSound

{-
step :: Float -> World -> IO World
step f w0 = do
    w1 <- wUpdateDisplay w0
    w2 <- wDraw w1
    wSound w2
    w3 <- return $ wCycleC8 w2
    w4 <- return $ wUpRefresh w3
    return w4
-}   

-- put this in the StateT monad and update Chip8 refresh var??
wRefreshUpd :: World -> World
wRefreshUpd w = 
    w { refreshCtr = (rfCtr + 1) `mod` rfMod }
  where
    rfCtr = refreshCtr w
    rfMod = refreshMod w

wRunC8 :: World -> World
wRunC8 w = w { chip8 = nextCycle c8 } 
  where
    c8 = chip8 w
      

wDisplayUpd :: StateT World IO ()
wDisplayUpd = do
    rfCtr <- gets refreshCtr
    case rfCtr of
        0 -> do
            pxArr <- gets pxArray
            pic   <- lift $ pixelPicture pxArr
            modify $ \w -> w {display = pic}
        _ ->
            return ()        

wSound :: StateT World IO ()
wSound = do
    st <- (stGet . chip8) <$> get 
    lift $ when (st > 0) soundEffect

wDraw :: World -> IO World
wDraw w =
    case (displayGet (chip8 w)) of
        Draw (Just sprite) -> do
            collision <- writeSprite arr sprite
            return $ w { chip8 = vfSet' c8 collision }
        Draw (Nothing) -> do
            return w
        Clear -> do
            arr' <- mkPixelArray
            return $ w {pxArray = arr'}        
  where
    arr = pxArray w
    c8  = chip8 w        
            


-------------------------------------------------------------------------------
-- Effects
-------------------------------------------------------------------------------

draw :: World -> IO G.Picture
draw w = return $ display w

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
