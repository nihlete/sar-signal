module Signal () where

import Data.Complex

data Signal = Signal
  { signalFc :: Double, -- carrier frequency [Hz]
    signalFs :: Double, -- sample rate [Hz]
    signalTau :: Double -- code intelval length [s]
  }

i :: Complex Double
i = 0 :+ 1.0

createSequence s c = undefined where
    f t = exp (i * 2*pi* f * t + u t)
    u t = 
    f = signalFc s
    t = [0,1/(signalFs s)..]
    