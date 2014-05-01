module Chip8 where

import Data.List
import Data.Maybe
import Control.Monad.State.Lazy
import Data.Word
import Data.IntMap.Lazy as M

type Address   = Word16

type OpCode    = Word16

type RAM       = M.IntMap Word8

type RegN      = Word8

type Registers = M.IntMap Word8

type I         = Address

type DT        = Word8

type ST        = Word8 

type PC        = Address

type Stack     = [Address]

data Chip8 = C8 {
    regs  :: Registers,
    i     :: I,
    dt    :: DT,
    st    :: ST,
    pc    :: PC,
    stack :: Stack,
    ram   :: RAM
} deriving Show

-- Construction
mkChip8 :: Chip8
mkChip8 = C8 {
    regs  = M.empty, 
    i     = 0x000, 
    dt    = 0x00, 
    st    = 0x00, 
    pc    = 0x000, 
    stack = [], 
    ram   = M.empty
}

-- Load program/program data in contiguous addresses
loadData :: Chip8 -> Address -> [Word8] ->Chip8
loadData c8 k [] = c8
loadData c8 k (x:xs) = loadData (setMem c8 k x) (k+1) xs

-- Registers
setReg :: Chip8 -> RegN -> Word8 -> Chip8
setReg _  k _ | k < 0x0 || k > 0xF = error "Can only update registers 0-F"
setReg c8 k x = c8 {regs = M.insert (fromIntegral k) x (regs c8)} 

getReg :: Chip8 -> RegN -> Word8
getReg _  k | k < 0x0 || k > 0xF = error "Can only retrieve registers 0-F"
getReg c8 k = M.findWithDefault 0x00 (fromIntegral k) $ regs c8


-- Special Registers
getI :: Chip8 -> I
getI = i

setI :: Chip8 -> Address -> Chip8
setI c8 k = c8 {i = k}

getPC :: Chip8 -> PC
getPC = pc

setPC :: Chip8 -> Address -> Chip8
setPC c8 k = c8 {pc = k}

setDT :: Chip8 -> Word8 -> Chip8
setDT c8 x = c8 {dt = x}

getDT :: Chip8 -> Word8
getDT = dt

setST :: Chip8 -> Word8 -> Chip8
setST c8 x = c8 {st = x}

getST :: Chip8 -> Word8
getST = st 

-- RAM
setMem :: Chip8 -> Address -> Word8 -> Chip8
setMem _  n _ | n < 0x000 || n > 0xFFF = error "Can only update addresses 0x000-0xFFF"
setMem c8 n x = c8 {ram = M.insert (fromIntegral n) x (ram c8)}

getMem :: Chip8 -> Address -> Word8
getMem _  n | n < 0x000 || n > 0xFFF = error "Can only retrieve addresses 0x000-0xFFF"
getMem c8 n = M.findWithDefault 0x000 (fromIntegral n) $ ram c8

-- Stack
peak :: Chip8 -> Address
peak = peak' . stack
  where 
    peak' []     = error "Cannot peak on empty stack"
    peak' (x:_) = x

pop :: Chip8 -> Chip8
pop c8 = c8 {stack = (pop' $ stack c8)}  
  where
    pop' []     = error "Cannot pop empty stack"
    pop' (_:xs) = xs

push :: Chip8 -> Address -> Chip8
push c8 n = c8 {stack = push' (stack c8) n}
  where
    push' xs _ | (length xs == 16) = error "Chip-8 stack can only be 16 deep"
    push' xs x = x:xs


-- Memory Addresses
chip8Start, chip8End, progStart, progEnd :: Address
-- Interpreter RAM Space
chip8Start = 0x000
chip8End   = 0x1FF
-- Program RAM Space
progStart  = 0x200
progEnd    = 0xFFF

