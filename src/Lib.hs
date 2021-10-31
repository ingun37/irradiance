{-# LANGUAGE PackageImports #-}

module Lib
  ( rotatedHemisphere,
    readHDRBytes,
    cubicals,
    computeIrradiance,
    theConvert
  )
where

import Codec.Picture

import Control.Monad
import qualified Data.ByteString as B
import Data.Either
import Data.Ix
import Linear.Metric
import Linear.Quaternion
import Linear.V2
import Linear.V3
import Linear.V4
import Linear.Vector
-- import Optics (Each (each), iover, (%~), (&))
import Util
import "lens" Control.Lens

tau = pi * 2

divBy :: (Integral a, Integral b) => a -> b -> Double
divBy y x = fromIntegral x / fromIntegral y

consequ :: Int -> [Int]
consequ n = range (0, n -1)

-- numbers n = consequ n & each %~ divBy n & each %~ subtract 0.5
numbers n = map (subtract 0.5 . divBy n) (consequ n)

-- pairs n = numbers n & each %~ (\x -> reverse $ numbers n & each %~ V2 x)
pairs n = map (\x -> reverse $ map (V2 x) (numbers n)) (numbers n)

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

normalizedV3Cube n = map (map (map normalize)) (theCube n)

radianceWeightOfPolarAngle x = cos x * sin x

rotatedHemisphere segments v =
  let rotator = rotate (rotationFromAVectorToAnother (V3 0 1 0) v)
      hemi = sphericalHemiSphere segments
      f = \radianceSphericalCoord ->
        let (V3 x y z) = rotator $ physicsCoord2GraphicsCoord $ sphericalToPhysicsCoord radianceSphericalCoord
            polar = view _x radianceSphericalCoord
            w = cos polar * sin polar
         in V4 x y z w
   in map f hemi

computeIrradiance :: Int -> Image PixelRGBF -> V3 Double -> V3 Double
computeIrradiance n img v =
  let hemi = sphericalHemiSphere n
      hemiLen = length hemi
      rotator = rotate (rotationFromAVectorToAnother (V3 0 1 0) v)
      radiances =
        map
          ( \scoord ->
              let vec = rotator ((physicsCoord2GraphicsCoord . sphericalToPhysicsCoord) scoord)
                  sampled = sampleEquirect img vec
                  (V2 fcoordX fcoordY) = scoord
                  polar = fcoordX
               in sampled ^* cos polar ^* sin polar
          )
          hemi
      irradiance = sumV radiances ^/ fromIntegral hemiLen
   in irradiance ^* pi

-- hemi = map (sample img . rotator) (hemiSphere 10)
-- sum = sumV hemi
-- average = sum ^/ fromIntegral (length hemi)
-- sampleEquirect img $ 12 *^ (rotate (rotationFromAVectorToAnother (V3 0 1 0) v) (V3 0 1 0))

-- in v
-- in V3 0 (sph^._y / pi) 0


v3ToRGBF :: V3 Double -> PixelRGBF
v3ToRGBF v3 =
  let V3 x y z = realToFrac <$> v3
   in PixelRGBF x y z

convertLinearToImage n square = generateImage (\i j -> v3ToRGBF ((square !! j) !! i)) n n

readHDRBytes =
  fromRight undefined
    . ( decodeHDR
          >=> ( \x -> case x of
                  ImageRGBF i -> Right i
                  _ -> undefined
              )
      )

theConvert cubeMapSize hdrBytes strategy =
  let v3Cube = normalizedV3Cube cubeMapSize
      img = readHDRBytes hdrBytes
      v3Cube' = map (map (map (strategy img))) v3Cube
  in map (convertLinearToImage cubeMapSize) v3Cube'