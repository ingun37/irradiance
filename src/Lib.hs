module Lib
  ( someFunc,
    rotatedHemisphere,
  )
where

import Codec.Picture
-- import Control.Lens (set, (^.))
import Control.Monad
import qualified Data.ByteString as B
import Data.Either
import Data.Ix
import Linear.Metric
import Linear.Quaternion
import Linear.V2
import Linear.V3
import Linear.Vector
import Optics (Each (each), iover, (%~), (&))
import Util

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

data Cubical = Px | Nx | Py | Ny | Pz | Nz deriving (Show)

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

theFeriHemi = sphericalHemiSphere 30

theFeriHemiLen = length theFeriHemi

rotatedHemisphere segments v =
  let rotator = rotate (rotationFromAVectorToAnother (V3 0 1 0) v)
      hemi = sphericalHemiSphere segments
   in map (rotator . physicsCoord2GraphicsCoord . sphericalToPhysicsCoord) hemi

computeIrradiance :: Image PixelRGBF -> V3 Double -> V3 Double
computeIrradiance img v =
  let aa = 0
      rotator = rotate (rotationFromAVectorToAnother (V3 0 1 0) v)
      radiances =
        map
          ( \scoord ->
              let vec = rotator ((physicsCoord2GraphicsCoord . sphericalToPhysicsCoord) scoord)
                  sampled = sampleEquirectWithNormalVector img vec
                  (V2 fcoordX fcoordY) = scoord
                  polar = fcoordX
               in sampled ^* cos polar ^* sin polar
          )
          theFeriHemi
      irradiance = sumV radiances ^/ fromIntegral theFeriHemiLen
   in irradiance ^* pi

-- hemi = map (sample img . rotator) (hemiSphere 10)
-- sum = sumV hemi
-- average = sum ^/ fromIntegral (length hemi)
-- sampleEquirectWithNormalVector img $ 12 *^ (rotate (rotationFromAVectorToAnother (V3 0 1 0) v) (V3 0 1 0))

-- in v
-- in V3 0 (sph^._y / pi) 0

theIrradianceCube img n = map (map (map (computeIrradiance img))) (theNormalizedCube n)

v3ToRGBF :: V3 Double -> PixelRGBF
v3ToRGBF v3 =
  let V3 x y z = realToFrac <$> v3
   in PixelRGBF x y z

theIrradianceImages equirect n = map (\square -> generateImage (\i j -> v3ToRGBF ((square !! j) !! i)) n n) (theIrradianceCube equirect n)

makeFileName :: Int -> FilePath
makeFileName i = show (cubicals !! i) ++ ".hdr"

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
  let images = theIrradianceImages img 64
  let aaa = iover each (\i a -> writeHDR (makeFileName i) a) images
  bb <- sequence aaa
  print bb