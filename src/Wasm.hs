module Wasm () where
import qualified Lib
import Linear.V3
-- import Asterius.Types
-- import Asterius.ByteString

rotatedHemisphere :: Int -> Double -> Double -> Double -> [Double]
rotatedHemisphere a x y z = Lib.rotatedHemisphere a (V3 x y z) >>= (\(V3 x y z) -> [x, y, z])

-- foreign export javascript "rotatedHemisphere" rotatedHemisphere :: Int -> Double -> Double -> Double -> [Double]
-- main :: IO ()
-- main = return ()