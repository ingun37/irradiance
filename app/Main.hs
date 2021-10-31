module Main where

import Lib
import qualified Data.ByteString as B
import Codec.Picture
import Optics


makeFileName :: Int -> FilePath
makeFileName i = show (cubicals !! i) ++ ".hdr"

main :: IO ()
main = do
  return ()