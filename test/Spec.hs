import Control.Monad
import Data.Foldable (traverse_)
import Linear.V3
import Util
import Linear.Quaternion
import qualified Lib
import qualified Data.ByteString as B
import Codec.Picture.Types (toneMapping, gammaCorrection)
import Codec.Picture

import Optics

makeFileName :: Int -> FilePath
makeFileName i = show (Lib.cubicals !! i) ++ ".png"

printv :: V3 Double -> IO ()
printv (V3 x y z) = putStrLn $ "new Vector3(" ++ show x ++ "f," ++ show y ++ "f," ++ show z ++ "f),"

main :: IO ()
main = do
  imgBytes <- B.readFile "venetian_crossroads_1k.hdr"
  let hdrs = Lib.convertToCubeMap sampleEquirect 64 imgBytes
  let tonemapped = map (gammaCorrection 2.2 . toneMapping 2) hdrs
  let pngs = map (convertRGB8 . ImageRGBF) tonemapped
  let aaa = iover each (\i a -> writePng (makeFileName i) a) pngs
  bb <- sequence aaa
  print bb
  return ()