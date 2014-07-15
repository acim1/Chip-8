module Chip8.IO.Run where

import Chip8
import Chip8.IO.Graphics
import qualified Data.ByteString as BS
import Graphics.Gloss
import System.Random

data Parameters = Parms FilePath
data World = World {
    chip8   :: Chip8,
    pxs     :: PixelArray,
    pic     :: Picture,
    refresh :: Bool
}

run :: IO ()
run = do
    w <- initialize
    putStrLn "Loaded."

effects :: World -> IO Picture
effects = undefined    

initialize :: IO World
initialize = do
    (Parms fp) <- getParameters
    program    <- readData fp
    g          <- getStdGen
    pxs        <- mkPixelArray
    let c8 = loadData (mkChip8 g) 0x200 program
    return $ World c8 pxs Blank False

getParameters :: IO Parameters
getParameters = do
   putStr "Chip-8 program filepath: "
   path <- getLine
   return $ Parms path
   
readData :: FilePath -> IO [Byte]
readData fp = do
    contents <- BS.readFile fp
    return $ BS.unpack contents

defaultClockSpeed :: Int
defaultClockSpeed = 1760000 -- 1.76 MHz
