module Main where

import Lib
import qualified Data.ByteString as B
import Codec.Picture
import Optics


makeFileName :: Int -> FilePath
makeFileName i = show (cubicals !! i) ++ ".hdr"

main :: IO ()
main = do
  imgBytes <- B.readFile "venetian_crossroads_1k.hdr"
  let img = readHDRBytes imgBytes
  let images = theIrradianceImages img 64
  let aaa = iover each (\i a -> writeHDR (makeFileName i) a) images
  bb <- sequence aaa
  print bb
