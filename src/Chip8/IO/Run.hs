module Chip8.IO.Run where

import Chip8
import Chip8.IO.Graphics
import qualified Data.ByteString as BS
import Graphics.Gloss
import System.Random

data Parameters = Parms FilePath

run :: IO ()
run = do
    initialize
    putStrLn "Loaded."

initialize :: IO (Chip8, PixelArray)
initialize = do
    (Parms fp) <- getParameters
    program    <- readData fp
    g          <- getStdGen
    arr        <- mkPixelArray
    let c8 = loadData (mkChip8 g) 0x200 program
    return (c8,arr)

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
