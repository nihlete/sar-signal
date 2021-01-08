module Main where

import Convolution (cacf)
import Data.Complex (Complex (..))
import Linear.V3
import MLS (mls)

j :: RealFloat a => Complex a
j = 0 :+ 1

main :: IO ()
main = print code

code = mls [True, False, True, False, False, True, False, False, True, False, False, True] [True, False, False, False, False, False, False, False, False, False, False]

carrierFeq :: Double
carrierFeq = 1e10

codeInterval :: Double 
codeInterval = 1e-6

df :: Double
df = (carrierFeq + fromIntegral (length code) / codeInterval) * 3 

signal :: Double -> Complex Double
signal t = a t * exp (- j * (2 * pi * carrierFeq * t :+ 0) )
  where
    i = floor (t / codeInterval) `mod` length code
    a t =
      if code !! i
        then 1
        else -1

scene :: [V3 Double]
scene = [V3 0 10000 0]

zeroPoint :: V3 Double
zeroPoint = V3 0 0 0

vel :: V3 Double
vel = V3 1 0 0

carrierPos :: V3 Double -> V3 Double
carrierPos t = zeroPoint + vel * t
