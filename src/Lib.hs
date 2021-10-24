module Lib
  ( someFunc,
  )
where

import Codec.Picture
import Control.Lens ((^.))
import Control.Monad
import qualified Data.ByteString as B
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

pairs n = numbers n & each %~ (\x -> reverse $ numbers n & each %~ V2 x)

addZComp z (V2 x y) = V3 x y z

nySquare n = map (map (addZComp (-0.5))) (pairs n)

rotSquare sq i j k r n =
  let q = axisAngle (V3 i j k) r :: Quaternion Double
      s = sq n
   in map (map (rotate q)) s

quarter = tau / 4

pzSquare = rotSquare nySquare 0 1 0 quarter


pySquare = rotSquare pzSquare 0 1 0 quarter

pxSquare = rotSquare pzSquare 0 0 1 quarter

nzSquare = rotSquare pxSquare 0 0 1 quarter

nxSquare = rotSquare nzSquare 0 0 1 quarter


-- pySquare = rotSquare nySquare 1 0 0 (-pi)

-- -- nxSquare = rotSquare 0 1 0 (tau / 4)

-- pxSquare = rotSquare pzSquare 0 1 0 (tau / 4)
-- nzSquare = rotSquare pxSquare 0 1 0 (tau / 4)
-- nxSquare = rotSquare nzSquare 0 1 0 (tau / 4)

-- -- pySquare = rotSquare 1 0 0 (tau / 4)

-- -- nySquare = rotSquare 1 0 0 (- tau / 4)

data Cubical = Px | Nx | Py | Ny | Pz | Nz deriving Show
cubicals = [Px, Nx, Py, Ny, Pz, Nz]

mapIntToSquare :: Cubical -> Int -> [[V3 Double]]
mapIntToSquare n = case n of
  Px -> pxSquare
  Nx -> nxSquare
  Py -> pySquare
  Ny -> nySquare
  Pz -> pzSquare
  Nz -> nzSquare

squareMakers = map mapIntToSquare cubicals

theCube n = map ($ n) squareMakers

theNormalizedCube n = map (map (map normalize)) (theCube n)

withInTau x = mod' (x + tau) tau

normalVToSpherical :: V3 Double -> V2 Double
normalVToSpherical v =
  let (V3 x y z) = v
      polar = atan2 y x
      azimuth = atan2 (norm (v ^. _xy)) z
   in V2 (withInTau polar) (withInTau azimuth)

sphericalToUV :: V2 Double -> V2 Double
sphericalToUV (V2 polar azimuth) = V2 (polar / tau) (azimuth / pi)

computeIrradiance :: Image PixelRGBF -> V3 Double -> V3 Double
computeIrradiance img v =
  let sph = normalVToSpherical v
      uv = sphericalToUV sph
      w = imageWidth img
      h = imageHeight img
      i = min (w -1) (floor (uv ^. _x * fromIntegral w))
      j = min (h -1) (floor (uv ^. _y * fromIntegral h))
      px = pixelAt img i j
      (PixelRGBF r g b) = px
   in fmap realToFrac (V3 r g b)

-- in v
-- in V3 0 (sph^._y / pi) 0

theIrradianceCube img n = map (map (map (computeIrradiance img))) (theNormalizedCube n)

v3ToRGBF :: V3 Double -> PixelRGB8
v3ToRGBF v3 =
  let v3' = fmap (floor . max 0 . min 255 . (* 255)) v3
      V3 x y z = v3'
   in PixelRGB8 x y z

theIrradianceImages equirect n = map (\square -> generateImage (\i j -> v3ToRGBF ((square !! j) !! i)) n n) (theIrradianceCube equirect n)

makeFileName :: Int -> FilePath
makeFileName i = show (cubicals !! i) ++ ".png"

readHDRBytes =
  fromRight undefined
    . ( decodeHDR
          >=> ( \x -> case x of
                  ImageRGBF i -> Right i
                  _ -> undefined
              )
      )

someFunc :: IO ()
someFunc = do
  imgBytes <- B.readFile "venetian_crossroads_1k.hdr"
  let img = readHDRBytes imgBytes
  let images = theIrradianceImages img 128
  let aaa = iover each (\i a -> writePng (makeFileName i) a) images
  bb <- sequence aaa
  print bb