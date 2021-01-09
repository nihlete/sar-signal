module Echo where

import Data.Complex
import Linear.Metric
import Linear.V3

echo :: (Floating a) => (a -> Complex a) -> V3 a -> V3 a -> a -> Complex a
echo signal carrier obj t = signal (t - d / c)
  where
    d = distance carrier obj
    c = 299792458