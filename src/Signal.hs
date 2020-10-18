module Signal () where

import Data.Complex

data SignalProp = SignalProp
  { signalA :: Double, -- amplitude
    signalFc :: Double, -- carrier frequency [Hz]
    signalTau :: Double -- code intelval length [s]
  }

i :: Complex Double
i = 0 :+ 1.0

signal :: SignalProp -> [Double] -> Double -> Complex Double
signal prop sequence t = a * exp (i * w * (t :+ 0) + u (t))
  where
    w = (2 * pi * signalFc prop) :+ 0
    tau = signalTau prop
    a = (signalA prop) :+ 0
    u t = (sequence !! (floor $ t / tau)) :+ 0