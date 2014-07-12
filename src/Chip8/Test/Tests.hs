module Chip8.Test.Tests where

import Chip8
import Chip8.IO.Graphics
import Chip8.Processing
import Control.Monad.State
import Data.Maybe
import Debug.Trace
import System.Random
import Data.Array.IO

-- Test display simple sprite: Display "0" in position (0,0)
t1 = do
    arr <- mkPixelArray
    writeArray arr (0,0) True -- top left, should leave hole in "0" when it is xored onto screen
    g   <- getStdGen
    let c8 = mkChip8 g
    let (Draw (Just sprite)) = evalState t1h1 c8
    putStrLn $ show sprite
    b <- writeSprite arr sprite
    putStrLn $ "Collision: " ++ (show b)
    showDisplay arr

-- Test 1, Helper 1
t1h1 :: State Chip8 Display    
t1h1 =  do
    memUpdate 0x200 0xA0
    memUpdate 0x201 0x00
    memUpdate 0x202 0xD0
    memUpdate 0x203 0x15
    regUpdate 0x0 0x00
    regUpdate 0x1 0x00
    nextCycleST
    nextCycleST
    displayFetch
