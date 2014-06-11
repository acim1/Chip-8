module Chip8
( Chip8,
  mkChip8,
  loadData, 
  regSet,
  regGet,
  vfSet,
  vfGet,
  iSet,
  iGet,
  pcSet,
  pcGet,
  dtSet,
  dtGet,
  stSet,
  stGet,
  memSet,
  memGet,
  peak,
  pop,
  push,
  randgSet,
  randgGet,
  keyboardSet,
  keyboardGet,
  displaySet,
  displayGet,
  waitSet,
  waitGet,
  hexSpriteAddr,
  -- Types
  Byte,
  Address,
  OpCode,
  Vx,
  Vy,
  I,
  DT,
  ST,
  PC,
  Stack,
  Keyboard,
  X,
  Y,
  Pixel,
  Sprite,
  Draw (..)
) where

import Data.List
import Data.Maybe
import Data.Word
import Data.IntMap.Lazy as M
import System.Random

type Byte      = Word8

type Address   = Word16

type OpCode    = Word16

type RAM       = M.IntMap Byte

type Vx        = Byte

type Vy        = Byte

type Registers = M.IntMap Byte

type I         = Address

type DT        = Byte

type ST        = Byte 

type PC        = Address

type Stack     = [Address]

type Keyboard  = [Byte]

type X         = Int

type Y         = Int

type Pixel     = Bool

type Sprite    = ((X,Y),[[Pixel]])

data Draw      = Clear | Draw (Maybe Sprite) deriving (Show)

data Chip8 = C8 {
    regs     :: Registers,
    i        :: I,
    dt       :: DT,
    st       :: ST,
    pc       :: PC,
    stack    :: Stack,
    ram      :: RAM,
    randg    :: StdGen,
    keyboard :: Keyboard,
    display  :: Draw,
    wait     :: Bool -- wait for input
} deriving Show

-- Construction
mkChip8 :: StdGen -> Chip8
mkChip8 g = loadData c8 0x000 hexSprites  
  where c8 = C8 {
    regs      = M.empty, 
    i         = 0x000, 
    dt        = 0x00, 
    st        = 0x00, 
    pc        = 0x200, -- typical program starting address
    stack     = [], 
    ram       = M.empty,
    randg     = g, -- for opcodes which require randomness
    keyboard  = [], -- currently depressed keys
    display   = Draw Nothing,
    wait      = False
  }

-- Load program/program data in contiguous addresses
loadData :: Chip8 -> Address -> [Byte] -> Chip8
loadData c8 k [] = c8
loadData c8 k (x:xs) = loadData (memSet c8 k x) (k+1) xs

-- Registers
regSet :: Chip8 -> Vx -> Byte -> Chip8
regSet _  k _ | k < 0x0 || k > 0xF = error "Can only update registers 0-F"
regSet c8 k x = c8 {regs = M.insert (fromIntegral k) x (regs c8)} 

regGet :: Chip8 -> Vx -> Byte
regGet _  k | k < 0x0 || k > 0xF = error "Can only retrieve registers 0-F"
regGet c8 k = M.findWithDefault 0x00 (fromIntegral k) $ regs c8

vfSet :: Chip8 -> Byte -> Chip8
vfSet c8 x = regSet c8 0xF x

vfGet :: Chip8 -> Byte
vfGet c8 = regGet c8 0xF


-- Special Registers
iSet :: Chip8 -> Address -> Chip8
iSet c8 k = c8 {i = k}

iGet :: Chip8 -> I
iGet = i

pcSet :: Chip8 -> Address -> Chip8
pcSet c8 k = c8 {pc = k}

pcGet :: Chip8 -> PC
pcGet = pc

dtSet :: Chip8 -> Byte -> Chip8
dtSet c8 x = c8 {dt = x}

dtGet :: Chip8 -> Byte
dtGet = dt

stSet :: Chip8 -> Byte -> Chip8
stSet c8 x = c8 {st = x}

stGet :: Chip8 -> Byte
stGet = st 

-- RAM
memSet :: Chip8 -> Address -> Byte -> Chip8
memSet _  n _ | n < 0x000 || n > 0xFFF = error "Can only update addresses 0x000-0xFFF"
memSet c8 n x = c8 {ram = M.insert (fromIntegral n) x (ram c8)}

memGet :: Chip8 -> Address -> Byte
memGet _  n | n < 0x000 || n > 0xFFF = error "Can only retrieve addresses 0x000-0xFFF"
memGet c8 n = M.findWithDefault 0x000 (fromIntegral n) $ ram c8

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

-- Random Generator
randgSet :: Chip8 -> StdGen -> Chip8
randgSet c8 g = c8 {randg = g}

randgGet :: Chip8 -> StdGen
randgGet = randg

-- Keyboard
keyboardSet :: Chip8 -> Keyboard -> Chip8
keyboardSet c8 xs = c8 {keyboard = xs}

keyboardGet :: Chip8 -> Keyboard
keyboardGet = keyboard

-- Display
displaySet :: Chip8 -> Draw -> Chip8
displaySet c8 x = c8 {display = x}

displayGet :: Chip8 -> Draw
displayGet = display

-- Waiting for input
waitSet :: Chip8 -> Bool -> Chip8
waitSet c8 x = c8 {wait = x}

waitGet :: Chip8 -> Bool
waitGet = wait

-- Pre-loaded Hex Digit Sprites
hexSprites :: [Byte]
hexSprites =   [0xF0,0x90,0x90,0x90,0xF0, -- 0
                0x20,0x60,0x20,0x20,0x70, -- 1
                0xF0,0x10,0xF0,0x80,0xF0, -- 2
                0xF0,0x10,0xF0,0x10,0xF0, -- 3
                0x90,0x90,0xF0,0x10,0x10, -- 4
                0xF0,0x80,0xF0,0x10,0xF0, -- 5
                0xF0,0x80,0xF0,0x90,0xF0, -- 6
                0xF0,0x10,0x20,0x40,0x40, -- 7
                0xF0,0x90,0xF0,0x90,0xF0, -- 8
                0xF0,0x90,0xF0,0x10,0xF0, -- 9
                0xF0,0x90,0xF0,0x90,0x90, -- A
                0xE0,0x90,0xE0,0x90,0xE0, -- B
                0xF0,0x80,0x80,0x80,0xF0, -- C
                0xE0,0x90,0x90,0x90,0xE0, -- D
                0xF0,0x80,0xF0,0x80,0xF0, -- E
                0xF0,0x80,0xF0,0x80,0x80  -- F  
               ]

-- Starting address for a given sprite               
hexSpriteAddr :: Byte -> Address
hexSpriteAddr = (*5) . fromIntegral 

-------------------------------------------------------------------------------
-- Constants
-------------------------------------------------------------------------------

-- Memory Addresses
chip8Start, chip8End, progStart, progEnd :: Address
-- Interpreter RAM Space
chip8Start = 0x000
chip8End   = 0x1FF
-- Program RAM Space
progStart  = 0x200
progEnd    = 0xFFF

