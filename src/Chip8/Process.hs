module Chip8.Process
(
) where

import Chip8
import Data.List
import Data.Maybe
import Control.Monad.State.Lazy
import Data.Word
import Data.IntMap.Lazy as M
import System.Random

-- Executes one Chip8 instruction cycle
cycle :: Chip8 -> Chip8
cycle = undefined

-- Execute a single instruction
execute :: Chip8 -> OpCode -> Chip8
execute = undefined  
