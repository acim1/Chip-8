module Chip8.Test.Tests where

import Chip8
import Chip8.Graphics
import Chip8.Processing
import Control.Monad.State
import Data.Maybe
import Debug.Trace
import System.Random

-- Test display simple sprite: Display "0" in position (0,0)
t1 = do
    arr <- mkPixelArray
    g   <- getStdGen
    let c8 = mkChip8 g
    let (Draw (Just sprite)) = evalState t1h1 c8
    putStrLn $ show sprite
    writeSprite arr sprite
    showDisplay arr

-- Test 1, Helper 1
t1h1 :: State Chip8 Display    
t1h1 =  do
    memUpdate 0x200 0xA0
    memUpdate 0x201 0x00
    memUpdate 0x202 0xD0
    memUpdate 0x203 0x05
    nextCycleST
    nextCycleST
    displayFetch
