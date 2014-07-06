module Chip8.Test.Tests where

import Chip8
import Chip8.Graphics
import Chip8.Processing
import Control.Monad.State
import Data.Maybe
import Debug.Trace
import System.Random

-- Test display simple sprite: Display "0" in position (0,0)
test01 = do
    arr <- mkPixelArray
    g   <- getStdGen
    let c8 = mkChip8 g
    let (Draw (Just sprite)) = evalState helper01 c8
    putStrLn $ show sprite
    writeSprite arr sprite
    showDisplay arr


helper01 :: State Chip8 Display    
helper01 =  do
    memUpdate 0x200 0xA0
    memUpdate 0x201 0x00
    memUpdate 0x202 0xD0
    memUpdate 0x203 0x05
    nextCycleST
    nextCycleST
    displayFetch
