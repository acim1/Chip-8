module Chip8.Processing
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
nextCycle :: Chip8 -> Chip8
nextCycle = undefined
