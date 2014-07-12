module Chip8.IO.Run where

import Chip8
import qualified Data.ByteString as BS

data Parameters = Parms FilePath

run :: IO ()
run = undefined

getParameters :: IO Parameters
getParameters = do
   putStr "Chip-8 program filepath: "
   path <- getLine
   return $ Parms path
   
readData :: FilePath -> IO [Byte]
readData fp = do
    contents <- BS.readFile fp
    return $ BS.unpack contents
