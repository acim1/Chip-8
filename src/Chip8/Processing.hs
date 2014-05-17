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
decode oc = case uphi of
    0x000 ->
        case oc of
            0x00E0 -> 
                CLS
            0x00EE -> 
                RET
            _      ->
                SYS nnn
    0x1000 -> 
        JP nnn
    0x2000 ->
        CALL nnn
    0x3000 -> 
        SE x kk
    0x4000 ->
        SNE x kk
    0x5000 ->
        SE1 x y 
    0x6000 ->
        LD  x kk 
    0x7000 ->
        ADD x kk
    0x8000 ->
        case lolo of
            0x0000 ->
                LD1 x y
            0x0001 ->
                OR x y
            0x0002 ->
                AND x y
            0x0003 ->
                XOR x y
            0x0004 ->
                ADD1 x y
            0x0005 ->
                SUB x y
            0x0006 ->
                SHR x
            0x0007 ->
                SUBN x y
            0x000E ->
                SHL x
    0x9000 ->
        SNE1 x y
    0xA000 ->
        LD2 nnn
    0xB000 ->
        JP1 nnn
    0xC000 ->
        RND x kk
    0xD000 ->
        DRW x y n
    0xE000 ->
        case lo of
            0x009E ->
                SKP x
            0x00A1 ->
                SKNP x
    0xF000 ->
        case lo of
            0x0007 ->
                LD3 x
            0x000A ->
                LD4 x
            0x0015 ->
                LD5 x
            0x0018 ->
                LD6 x
            0x001E ->
                ADD2 x
            0x0029 ->
                LD7 x
            0x0033 ->
                LD8 x
            0x0055 ->
                LD9 x
            0x0065 ->
                LD10 x
  where 
    hi   = 0xFF00 .&. oc
    uphi = 0xF000 .&. oc
    lohi = 0x0F00 .&. oc
    uplo = 0x00F0 .&. oc
    lolo = 0x000F .&. oc
    lo   = 0x00FF .&. oc
    nnn  = 0x0FFF .&. oc
    x    = w8 $ shiftR (0x0F00 .&. oc) 8
    y    = w8 $ shiftR (0x00F0 .&. oc) 4
    kk   = w8 $ 0x00FF .&. oc
    n    = w8 $ 0x000F .&. oc

execute :: Chip8 -> Op -> Chip8
execute c8 op = undefined  

data Op =
    SYS Address       -- 0nnn
  | CLS               -- 00E0
  | RET               -- 00EE
  | JP Address        -- 1nnn
  | CALL Address      -- 2nnn
  | SE Vx Byte        -- 3xkk
  | SNE Vx Byte       -- 4xkk
  | SE1 Vx Vy         -- 5xy0
  | LD Vx Byte        -- 6xkk
  | ADD Vx Byte       -- 7xkk
  | LD1 Vx Vy         -- 8xy0
  | OR Vx Vy          -- 8xy1
  | AND Vx Vy         -- 8xy2
  | XOR Vx Vx         -- 8xy3
  | ADD1 Vx Vy        -- 8xy4
  | SUB Vx Vy         -- 8xy5
  | SHR Vx            -- 8xy6
  | SUBN Vx Vy        -- 8xy7
  | SHL Vx            -- 8xyE
  | SNE1 Vx Vy        -- 9xy0
  | LD2 Address       -- Annn
  | JP1 Address       -- Bnnn
  | RND Vx Byte       -- Cxkk
  | DRW Vx Vy Byte    -- Dxyn
  | SKP Vx            -- Ex9E
  | SKNP Vx           -- ExA1
  | LD3 Vx            -- Fx07
  | LD4 Vx            -- Fx0A
  | LD5 Vx            -- Fx15
  | LD6 Vx            -- Fx18
  | ADD2 Vx           -- Fx1E
  | LD7 Vx            -- Fx29
  | LD8 Vx            -- Fx33
  | LD9 Vx            -- Fx55
  | LD10 Vx           -- Fx65
          deriving (Show)

w8 :: (Integral a) => a -> Word8
w8 = fromIntegral

w16 :: (Integral a) => a -> Word16
w16 = fromIntegral

-- future instructions, Can expand defs to include nibble or word pieces, i.e. LD nibble
