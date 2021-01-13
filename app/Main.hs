module Main where

import Correlation (cacf)
import Data.Complex (Complex (..))
import Linear.Metric
import Linear.V3
import Linear.Vector
import MLS (mls)
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo

j :: RealFloat a => Complex a
j = 0 :+ 1

main :: IO ()
main = 
  -- do 
  -- print "IDDQD"
  -- print code
  toFile def "example1_big.png" $ do
    layout_title .= "Amplitude Modulation"
    setColors [opaque blue, opaque red]
    plot (line "am" [signal' [0,(0.5)..400]])
    plot (points "am points" (signal' [0,7..400]))


signal' :: [Double] -> [(Double,Double)]
signal' xs = [ (x,(sin (x*3.14159/45) + 1) / 2 * (sin (x*3.14159/5))) | x <- xs ]

code = mls [True, False, True, False, False, True, False, False, True, False, False, True] [True, False, False, False, False, False, False, False, False, False, False]

base = length code

carrierFeq :: Double
carrierFeq = 1e10

codeInterval :: Double
codeInterval = 1e-6

-- Теорема Котельникова
df :: Double
df = (carrierFeq + fromIntegral base / codeInterval) * 3

signal :: Double -> Complex Double
signal t
  | t >= 0 && t < signalLength = a t * exp (- j * (2 * pi * carrierFeq * t :+ 0))
  | otherwise = 0
  where
    signalLength = codeInterval * fromIntegral base
    i = floor (t / codeInterval) `mod` base
    a t =
      if code !! i
        then 1
        else -1

echo :: (Double -> V3 Double) -> Double -> Complex Double
echo obj t = signal (t - d / c)
  where
    d = distance (carrier t) (obj t)
    c = 3e8

scene :: [V3 Double]
scene = [V3 0 10000 0]

zeroPoint :: V3 Double
zeroPoint = V3 0 0 0

vel :: V3 Double
vel = V3 1 0 0

carrier :: Double -> V3 Double
carrier t = zeroPoint + vel ^* t
