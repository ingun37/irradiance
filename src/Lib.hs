module Lib
  ( someFunc,
  )
where

import Codec.Picture
import Codec.Picture.HDR
import Codec.Picture.Types (generateImage)
import Control.Lens ((^.))
import Control.Monad
import Data.Either
import Data.Fixed
import Data.Ix
import Data.Ratio
import Linear.Metric
import Linear.Quaternion
import Linear.V2
import Linear.V3
import Optics (Each (each), iover, (%~), (&))

tau = pi * 2

divBy :: (Integral a, Integral b) => a -> b -> Double
divBy y x = fromIntegral x / fromIntegral y

consequ :: Int -> [Int]
consequ n = range (0, n -1)

numbers n = consequ n & each %~ divBy n & each %~ subtract 0.5

pairs n = numbers n & each %~ (\x -> numbers n & each %~ V2 x)

addZComp z (V2 x y) = V3 x y z

nzSquare n = map (map (addZComp (-1))) (pairs n)

rotSquare i j k r n =
  let q = axisAngle (V3 i j k) r :: Quaternion Double
      s = nzSquare n
   in map (map (rotate q)) s

pzSquare = rotSquare 0 1 0 pi

nxSquare = rotSquare 0 1 0 (tau / 4)

pxSquare = rotSquare 0 1 0 (- tau / 4)

pySquare = rotSquare 1 0 0 (tau / 4)

nySquare = rotSquare 1 0 0 (- tau / 4)

theCube n = map ($ n) [pxSquare, nxSquare, pySquare, nySquare, pzSquare, nzSquare]

theNormalizedCube n = map (map (map normalize)) (theCube n)

normalVToSpherical :: V3 Double -> V2 Double
normalVToSpherical v =
  let (V3 x y z) = v
      polar = atan2 y x
      azimuth = atan2 (norm (v ^. _xy)) z
   in V2 polar azimuth

sphericalToUV :: V2 Double -> V2 Double
sphericalToUV = fmap (\x -> mod' x tau / tau)

computeIrradiance :: Image PixelRGBF -> V3 Double -> V3 Double
computeIrradiance img v =
  let uv = sphericalToUV $ normalVToSpherical v
      w = imageWidth img
      h = imageHeight img
      i = floor (uv ^. _x * fromIntegral w)
      j = floor (uv ^. _y * fromIntegral h)
      px = pixelAt img i j
      (PixelRGBF r g b) = px
   in fmap realToFrac (V3 r g b)

readHDR =
  fromRight undefined
    . ( decodeHDR
          >=> ( \x -> case x of
                  ImageRGBF i -> Right i
                  _ -> undefined
              )
      )

theIrradianceCube img n = map (map (map (computeIrradiance img))) (theNormalizedCube n)

v3ToRGBF :: V3 Double -> PixelRGB8
v3ToRGBF v3 =
  let v3' = fmap (floor . max 0 . (* 255)) v3
      V3 x y z = v3'
   in PixelRGB8 x y z

theIrradianceImages equirect n = map (\square -> generateImage (\i j -> v3ToRGBF ((square !! i) !! j)) n n) (theIrradianceCube equirect n)

makeFileName :: Int -> FilePath
makeFileName i = show i ++ ".png"

someFunc :: IO ()
someFunc = do
  let images = theIrradianceImages 32
  let aaa = iover each (\i a -> writePng (makeFileName i) a) images
  bb <- sequence aaa
  print bb