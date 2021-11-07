module Main where

import Lib
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
  -- let strategy = sampleEquirect
  let strategy = computeIrradiance 30
  let images = convertToCubeMap strategy 32 imgBytes
  let aaa = iover each (\i a -> writeHDR (makeFileName i) a) images
  bb <- sequence aaa
  print bb
