module Wasm (main) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BZ
import GHC.Float (double2Float, float2Double)
import qualified Lib
import Util
import Linear.V4
import Linear.V3
import Data.Binary.Put
import Data.ByteString.Lazy.Char8 (toStrict)
import Codec.Picture.Types
import Codec.Picture
import Data.Bson
import Data.Bson.Binary
import Data.Text (pack)

-- import Asterius.Types
-- import Asterius.ByteString

data JSUint8Array = JSUint8Array -- comment out

byteStringToJSUint8Array :: B.ByteString -> JSUint8Array -- comment out
byteStringToJSUint8Array = undefined -- comment out

byteStringFromJSUint8Array :: JSUint8Array -> B.ByteString -- comment out
byteStringFromJSUint8Array = undefined -- comment out

rotatedHemisphere :: Int -> Double -> Double -> Double -> JSUint8Array
rotatedHemisphere a x y z =
  let h = Lib.rotatedHemisphere a (V3 x y z)
      doubles = h >>= (\(V4 x y z w) -> [x, y, z, w])
      floats = map (runPut . putFloatbe . double2Float) doubles
      floatsBS = toStrict $ BZ.concat floats
   in byteStringToJSUint8Array floatsBS

(.***) = (.) . (.) . (.)
(.**) = (.) . (.)

convertToPngCubeMap cubeMapSize hdrUInt8Array = 
   let hdrs = Lib.convertToCubeMap sampleEquirect cubeMapSize (byteStringFromJSUint8Array hdrUInt8Array)
       pngs = map (Binary . toStrict . encodePng . convertRGB8 . ImageRGBF . gammaCorrection 2.2 . toneMapping 2) hdrs
       field = pack "pngs" := val pngs
       aaa = toStrict $ runPut $ putDocument [field]
   in byteStringToJSUint8Array aaa

-- foreign export javascript "rotatedHemisphere" rotatedHemisphere :: Int -> Double -> Double -> Double -> JSUint8Array
-- foreign export javascript "convertToPngCubeMap" convertToPngCubeMap :: Int -> JSUint8Array -> JSUint8Array

main :: IO ()
main = return ()