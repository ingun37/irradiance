module Util
  ( rotationFromAVectorToAnother,
    sphericalToUV,
    fericalHemiSphere,
    sphericalToVec,
    vecToSpherical,
    sampleEquirectWithNormalVector,
    fericalToVec
  )
where

import Codec.Picture
import Control.Lens
import Data.Fixed
import Data.Ix
import Linear.Metric
import Linear.Quaternion
import Linear.V2
import Linear.V3
import Linear.Vector

tau = pi * 2

quarter = tau / 4

rotationFromAVectorToAnother :: V3 Double -> V3 Double -> Quaternion Double
rotationFromAVectorToAnother v1 v2 =
  let c = cross v1 v2
      w = dot v1 v2 + 1
      q = Quaternion w c
   in normalize q

-- fericalHemiSphere :: Int -> [V2 Double]
-- fericalHemiSphere step = do
--   let circumStep = step * 4
--   let delta = tau / fromIntegral circumStep
--   i <- range (0, circumStep -1)
--   let azimuth = fromIntegral i * delta
--   j <- range (0, step -1)
--   let polar = fromIntegral j * delta
--   return $ V2 polar azimuth

fericalHemiSphere :: Int -> [V2 Double]
fericalHemiSphere step = [V2 0.1 0, V2 0.1 quarter, V2 0.1 pi, V2 0.1 (pi + quarter)]

sphericalToUV :: V2 Double -> V2 Double
sphericalToUV (V2 polar azimuth) = V2 (polar / tau) (azimuth / pi)

fericalToVec :: V2 Double -> V3 Double
fericalToVec v =
  let (V3 x y z) = sphericalToVec v
   in V3 y z x

sphericalToVec :: V2 Double -> V3 Double
sphericalToVec (V2 polar azimuth) = V3 (sin polar * cos azimuth) (sin azimuth * sin polar) (cos polar)

withInTau x = mod' (x + tau) tau

vecToSpherical :: V3 Double -> V2 Double
vecToSpherical v =
  let (V3 x y z) = v
      polar = atan2 y x
      azimuth = atan2 (norm (v ^. _xy)) z
   in V2 (withInTau polar) (withInTau azimuth)

sampleEquirectWithNormalVector :: Image PixelRGBF -> V3 Double -> V3 Double
sampleEquirectWithNormalVector img v =
  let sph = vecToSpherical v
      uv = sphericalToUV sph
      w = imageWidth img
      h = imageHeight img
      i = min (w -1) (floor (uv ^. _x * fromIntegral w))
      j = min (h -1) (floor (uv ^. _y * fromIntegral h))
      px = pixelAt img i j
      (PixelRGBF r g b) = px
   in fmap realToFrac (V3 r g b)