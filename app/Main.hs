module Main where

import Lib (theConvert, cubicals)
import Util (sampleEquirect)
import qualified Data.ByteString as B
import Codec.Picture
import Optics


makeFileName :: Int -> FilePath
makeFileName i = show (cubicals !! i) ++ ".hdr"

main :: IO ()
main = do
  imgBytes <- B.readFile "venetian_crossroads_1k.hdr"
  -- let img = readHDRBytes imgBytes
  -- let images = theIrradianceImages img 64
  -- let aaa = iover each (\i a -> writeHDR (makeFileName i) a) images
  -- bb <- sequence aaa
  let images = theConvert 64 imgBytes (sampleEquirect)
  let aaa = iover each (\i a -> writeHDR (makeFileName i) a) images
  bb <- sequence aaa
  print bb
