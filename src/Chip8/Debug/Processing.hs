module Chip8.Debug.Processing where

import Chip8
import Control.Monad.State
import Data.Bits
import Data.IntMap.Lazy as M
import Data.List
import Data.Maybe
import Data.Word
import System.Random

-- | Executes one Chip8 instruction cycle
nextCycle :: Chip8 -> Chip8
nextCycle c8 = exec . decode . fetch $ c8
  where exec op = execState (execute op) c8 

-- | Fectches opcode from current pc location
fetch :: Chip8 -> OpCode
fetch c8 = let pc    = pcGet c8
               byte1 = memGet c8 pc
               byte2 = memGet c8 (pc+1)
               oc    = shiftL (w16 byte1) 8
               oc'   = oc .|. (w16 byte2) 
           in oc'    

-- | Translates op codes into their operations and operands
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
        JP1 nnn
    0x2000 ->
        CALL nnn
    0x3000 -> 
        SE1 x kk
    0x4000 ->
        SNE1 x kk
    0x5000 ->
        SE2 x y 
    0x6000 ->
        LD1  x kk 
    0x7000 ->
        ADD1 x kk
    0x8000 ->
        case lolo of
            0x0000 ->
                LD2 x y
            0x0001 ->
                OR x y
            0x0002 ->
                AND x y
            0x0003 ->
                XOR x y
            0x0004 ->
                ADD2 x y
            0x0005 ->
                SUB x y
            0x0006 ->
                SHR x
            0x0007 ->
                SUBN x y
            0x000E ->
                SHL x
    0x9000 ->
        SNE2 x y
    0xA000 ->
        LD3 nnn
    0xB000 ->
        JP2 nnn
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
                LD4 x
            0x000A ->
                LD5 x
            0x0015 ->
                LD6 x
            0x0018 ->
                LD7 x
            0x001E ->
                ADD3 x
            0x0029 ->
                LD8 x
            0x0033 ->
                LD9 x
            0x0055 ->
                LD10 x
            0x0065 ->
                LD11 x
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

-- | Performs pre-execution maintenance and calls execute' routine to do main logic.
execute :: Op -> State Chip8 ()
execute op = do
    pcIncr
    dtDecr
    stDecr
    waitUpdate Nothing
    displayUpdate $ Draw Nothing
    execute' op

-- | Central logic routine
execute' :: Op -> State Chip8 ()
execute' op = 
    case op of
    --  SYS _ -> do
    --      undefined (Ignored by modern interpreters)
        CLS -> do
            displayUpdate Clear
        RET -> do
            addr <- popStack
            pcUpdate addr
        JP1 addr -> do
            pcUpdate addr
        CALL addr -> do
            pc <- pcFetch
            pushStack pc
            pcUpdate addr
        SE1 vx byte -> do
            x <- regFetch vx
            when (x == byte) pcIncr
        SNE1 vx byte -> do
            x <- regFetch vx
            when (x /= byte) pcIncr
        SE2 vx vy -> do
            x <- regFetch vx
            y <- regFetch vy
            when (x == y) pcIncr
        LD1 vx byte -> do
            regUpdate vx byte
        ADD1 vx byte -> do
            x <- regFetch vx
            regUpdate vx (x+byte)
        LD2 vx vy -> do
            y <- regFetch vy
            regUpdate vx y
        OR vx vy -> do
            x <- regFetch vx
            y <- regFetch vy
            regUpdate vx $ x .|. y
        AND vx vy -> do
            x <- regFetch vx
            y <- regFetch vy
            regUpdate vx $ x .&. y
        XOR vx vy -> do
            x <- regFetch vx
            y <- regFetch vy
            regUpdate vx $ x `xor` y
        ADD2 vx vy -> do
            x <- regFetch vx
            y <- regFetch vy
            let xy = (w16 x) + (w16 y)
            vfUpdate $ xy > 0xFF
            regUpdate vx $ w8 xy
        SUB vx vy -> do
            x <- regFetch vx
            y <- regFetch vy
            vfUpdate $ x > y
            regUpdate vx $ x - y
        SHR vx -> do
            x <- regFetch vx
            vfUpdate $ testBit x 0
            regUpdate vx $ shiftR x 1
        SHL vx -> do
            x <- regFetch vx
            vfUpdate $ testBit x 7
            regUpdate vx $ shiftL x 1
        SNE2 vx vy -> do
            x <- regFetch vx
            y <- regFetch vy
            when (x /= y) pcIncr
        LD3 addr -> do
            iUpdate addr
        JP2 addr -> do
            x <- regFetch 0x00
            pcUpdate $ addr + (w16 x)
        RND vx byte -> do
            g <- randgFetch
            let (rbyte,g') = randomR (0,255) g
            randgUpdate g'
            regUpdate vx $ rbyte .&. byte
        DRW vx vy n -> do
            x <- regFetch vx
            y <- regFetch vy
            i      <- iFetch
            pixels <- drawFrom i n
            displayUpdate $ Draw $ Just ((int x, int y),pixels)
            -- VF collision update handled IO side
        SKP vx -> do
            x  <- regFetch vx
            kb <- kbFetch
            when (elem x kb) pcIncr
        SKNP vx -> do
            x  <- regFetch vx
            kb <- kbFetch
            when (notElem x kb) pcIncr
        LD4 vx -> do
            dt <- dtFetch
            regUpdate vx dt
        LD5 vx -> do
            waitUpdate $ Just vx
            -- Register set to waited for key press handled IO side
        LD6 vx -> do
            x <- regFetch vx
            dtUpdate x
        LD7 vx -> do
            x <- regFetch vx
            stUpdate x
        ADD3 vx -> do
            x <- regFetch vx
            i <- iFetch
            iUpdate $ i + (w16 x)
        LD8 vx -> do
            x <- regFetch vx
            iUpdate $ hexSpriteAddr x
        LD9 vx -> do
            x <- regFetch vx
            let (hdrs,r0)  = vx `divMod` 100
            let (tens,r1)  = r0 `divMod` 10
            let  ones      = r1
            i <- iFetch
            memUpdate i hdrs
            memUpdate (i+1) tens
            memUpdate (i+2) ones
        LD10 vx -> do
            i <- iFetch
            foldM_ 
                (\i' vy -> do {y <- regFetch vy; memUpdate i' y; return (i'+1);}) 
                i 
                [0x0..vx]
        LD11 vx -> do
            i <- iFetch
            foldM_ 
                (\i' vy -> do {y <- memFetch i'; regUpdate vy y; return (i'+1);}) 
                i 
                [0x0..vx]

             

-------------------------------------------------------------------------------
-- State Update Methods
-------------------------------------------------------------------------------

-- PC
pcIncr :: State Chip8 ()
pcIncr = state $ \c8 ->
         let pc = pcGet c8
             c8'= pcSet c8 (pc+2) 
         in ((),c8')

pcUpdate :: Address -> State Chip8 ()
pcUpdate x = state $ \c8 -> 
             ((), pcSet c8 x)
             
pcFetch :: State Chip8 Address
pcFetch = state $ \c8 ->
          (pcGet c8,c8)         

-- DT
dtDecr :: State Chip8 ()
dtDecr = state $ \c8 -> 
         let dt = dtGet c8
             c8'= if dt > 0 
                  then dtSet c8 (dt-1)
                  else c8
         in ((),c8')

dtUpdate :: Byte -> State Chip8 ()
dtUpdate x = state $ \c8 ->
             ((),dtSet c8 x)

dtFetch :: State Chip8 Byte
dtFetch = state $ \c8 ->
          (dtGet c8,c8)                     

-- ST               
stDecr :: State Chip8 ()
stDecr = state $ \c8 -> 
         let st = stGet c8
             c8'= if st > 0 
                  then stSet c8 (st-1)
                  else c8
         in ((),c8')

stUpdate :: Byte -> State Chip8 ()
stUpdate x = state $ \c8 ->
             ((),stSet c8 x)

stFetch :: State Chip8 Byte
stFetch = state $ \c8 ->
          (stGet c8,c8)

-- I
iUpdate :: Address -> State Chip8 ()
iUpdate x = state $ \c8 ->
            ((),iSet c8 x)
            
iFetch :: State Chip8 I
iFetch = state $ \c8 ->
         (iGet c8,c8)    

-- Stack
popStack :: State Chip8 Address
popStack = state $ \c8 ->
            let c8' = pop c8
                x   = peak c8'
            in (x,c8')

pushStack :: Address -> State Chip8 ()
pushStack x = state $ \c8 ->
              ((),push c8 x)

-- General Purpose Registers
regUpdate :: Vx -> Byte -> State Chip8 ()
regUpdate k x = state $ \c8 ->
                ((),regSet c8 k x)

regFetch :: Vx -> State Chip8 Vx
regFetch k = state $ \c8 ->
             (regGet c8 k, c8)

-- | Updates flag register
vfUpdate :: Bool -> State Chip8 ()
vfUpdate p = do
    if p
        then regUpdate vf 0x01
        else regUpdate vf 0x00
  where vf = 0xF              

-- Memory
memUpdate :: Address -> Byte -> State Chip8 ()
memUpdate k x = state $ \c8 ->
                ((),memSet c8 k x)

memFetch :: Address -> State Chip8 Byte
memFetch k = state $ \c8 ->
             (memGet c8 k, c8)

-- Waiting for Input
waitUpdate :: Maybe Vx -> State Chip8 ()
waitUpdate x = state $ \c8 ->
               ((), waitSet c8 x)

-- Keyboard
kbFetch :: State Chip8 Keyboard
kbFetch = state $ \c8 ->
          (keyboardGet c8, c8)
          
-- Display
displayUpdate :: Display -> State Chip8 ()
displayUpdate x = state $ \c8 ->
                  ((),displaySet c8 x)

drawFrom :: Address -> Byte -> State Chip8 [PixelByte]
drawFrom addr n = state $ \c8 ->
                  (getPixels c8 addr n, c8)

getPixels :: Chip8 -> Address -> Byte -> [PixelByte]
getPixels c8 addr 0 = []
getPixels c8 addr n = [byte2Pixels $ memGet c8 (addr + k) | k <- [0..(n'-1)]] 
  where
    n' = w16 n
    byte2Pixels byte = zipWith testBit (repeat byte) lilEndianBits
    lilEndianBits = reverse [0..7]  
 
                  
-- Randomness
randgFetch :: State Chip8 StdGen
randgFetch = state $ \c8 ->
             (randgGet c8, c8)
             
randgUpdate :: StdGen -> State Chip8 ()
randgUpdate g = state $ \c8 ->
                ((),randgSet c8 g)               
 
data Op =
    SYS Address       -- 0nnn
  | CLS               -- 00E0
  | RET               -- 00EE
  | JP1 Address       -- 1nnn
  | CALL Address      -- 2nnn
  | SE1 Vx Byte       -- 3xkk
  | SNE1 Vx Byte      -- 4xkk
  | SE2 Vx Vy         -- 5xy0
  | LD1 Vx Byte       -- 6xkk
  | ADD1 Vx Byte      -- 7xkk
  | LD2 Vx Vy         -- 8xy0
  | OR Vx Vy          -- 8xy1
  | AND Vx Vy         -- 8xy2
  | XOR Vx Vx         -- 8xy3
  | ADD2 Vx Vy        -- 8xy4
  | SUB Vx Vy         -- 8xy5
  | SHR Vx            -- 8xy6
  | SUBN Vx Vy        -- 8xy7
  | SHL Vx            -- 8xyE
  | SNE2 Vx Vy        -- 9xy0
  | LD3 Address       -- Annn
  | JP2 Address       -- Bnnn
  | RND Vx Byte       -- Cxkk
  | DRW Vx Vy Byte    -- Dxyn
  | SKP Vx            -- Ex9E
  | SKNP Vx           -- ExA1
  | LD4 Vx            -- Fx07
  | LD5 Vx            -- Fx0A
  | LD6 Vx            -- Fx15
  | LD7 Vx            -- Fx18
  | ADD3 Vx           -- Fx1E
  | LD8 Vx            -- Fx29
  | LD9 Vx            -- Fx33
  | LD10 Vx           -- Fx55
  | LD11 Vx           -- Fx65
          deriving (Show)


-- future instructions, Can expand defs to include nibble or word pieces, i.e. LD1 nibble
