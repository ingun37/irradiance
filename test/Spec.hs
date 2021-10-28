import Control.Monad
import Data.Foldable (traverse_)
import Linear.V3
import Util
import Linear.Quaternion

printv :: V3 Double -> IO ()
printv (V3 x y z) = putStrLn $ "new Vector3(" ++ show x ++ "f," ++ show y ++ "f," ++ show z ++ "f),"

main :: IO ()
main = do
  let toV = V3 1 1 (-1)
  let rotator = rotate (rotationFromAVectorToAnother (V3 0 1 0) toV)
  let aaa = rotator . fericalToVec <$> sphericalHemiSphere 10
  traverse_ printv aaa