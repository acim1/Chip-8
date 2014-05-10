module Chip8.Processing
(
) where

import Chip8
import Data.Bits
import Data.List
import Data.Maybe
import Control.Monad.State.Lazy
import Data.Word
import Data.IntMap.Lazy as M
import System.Random

-- Executes one Chip8 instruction cycle
nextCycle :: Chip8 -> Chip8
nextCycle c8 = exec . decode . fetch $ c8
  where exec = execute c8 

fetch :: Chip8 -> OpCode
fetch c8 = let pc    = pcGet c8
               byte1 = memGet c8 pc
               byte2 = memGet c8 (pc+1)
               opc   = shiftL  (fi byte1) 8
               opc'  = opc .|. (fi byte2) 
           in opc'    
  where fi x = fromIntegral x :: Word16

decode :: OpCode -> Op
decode = undefined

execute :: Chip8 -> Op -> Chip8
execute c8 op = undefined  

data Op = SYS -- 0nnn - SYS addr. Jump to a machine code routine at nnn.
-- future instructions, Can expand defs to include nibble or word pieces, i.e. LD nibble
