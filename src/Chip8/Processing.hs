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
               oc    = shiftL (w16 byte1) 8
               oc'   = oc .|. (w16 byte2) 
           in oc'    

decode :: OpCode -> Op
decode oc
    | oc ==  0x00E0 = 
        CLS
    | oc ==  0x00EE = 
        RET
    | oc .&. 0xF000 == 0x1000 = 
        JP (0x0FFF .&. oc)
    | oc .&. 0xF000 == 0x2000 =
        CALL (0x0FFF .&. oc)
    | oc .&. 0xF000 == 0x3000 = 
        SE (w8 $ shiftR (0x0F00 .&. oc) 8) (w8 $ 0x00FF .&. oc)
    | oc .&. 0xF000 == 0x4000 =
        SNE (w8 $ shiftR (0x0F00 .&. oc) 8) (w8 $ 0x00FF .&. oc)
    | oc .&. 0xF000 == 0x5000 =
        SE1 (w8 $ shiftR (0x0F00 .&. oc) 8) (w8 $ shiftR (0x00F0 .&. oc) 4)  
    | oc .&. 0xF000 == 0x6000 =
        LD  (w8 $ shiftR (0x0F00 .&. oc) 8) (w8 $ 0x00FF .&. oc)    

execute :: Chip8 -> Op -> Chip8
execute c8 op = undefined  

data Op =   
    CLS               -- 00E0
  | RET               -- 00EE
  | JP Address        -- 1nnn
  | CALL Address      -- 2nnn
  | SE Vx Byte        -- 3xkk
  | SNE Vx Byte       -- 4xkk
  | SE1 Vx Vy         -- 5xy0
  | LD Vx Byte        -- 6xkk - LD Vx, byte
          deriving (Show)

w8 :: (Integral a) => a -> Word8
w8 = fromIntegral

w16 :: (Integral a) => a -> Word16
w16 = fromIntegral

-- future instructions, Can expand defs to include nibble or word pieces, i.e. LD nibble
