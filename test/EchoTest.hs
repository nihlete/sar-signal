module EchoTest where

import Data.Complex
import Data.Vector.FFT
import qualified Data.Vector.Unboxed as Unboxed
import Echo
import Linear.V3
import Linear.Vector
import Test.Hspec
import Test.QuickCheck

echoTest :: SpecWith ()
echoTest = describe ("Echo signal " ++ (show $ length [0, 1 / fd .. 1e-5])) $ do
  it "no Doppler shift" $ do
    Unboxed.maxIndex (Unboxed.map magnitude harmonicSpectrum) `shouldBe` Unboxed.maxIndex (Unboxed.map magnitude echoSpectrum)
  it "with Doppler shift" $ do
    Unboxed.maxIndex (Unboxed.map magnitude harmonicSpectrum) `shouldNotBe` Unboxed.maxIndex (Unboxed.map magnitude echoSpectrumWithDoppler)

j :: RealFloat a => Complex a
j = 0 :+ 1

f0 = 1000000000.0

fd = 6.325 * f0

harmonicSignal t = exp (- j * (2 * pi * f0 * t :+ 0))

harmonicSpectrum = fft s
  where
    s = Unboxed.fromList $ map harmonicSignal [0, 1 / fd .. 1e-5]

echoSpectrum = fft e
  where
    e = Unboxed.fromList $ map (echo harmonicSignal zero (V3 0 10000 0)) [0, 1 / fd .. 1e-5]

echoSpectrumWithDoppler = fft e
  where
    e = Unboxed.fromList $ map s [0, 1 / fd .. 1e-5]
    s t = echo harmonicSignal (carrier t) (V3 0 10000 0) t

vel :: V3 Double
vel = V3 1000 1000 0

carrier :: Double -> V3 Double
carrier t = zero + vel ^* t