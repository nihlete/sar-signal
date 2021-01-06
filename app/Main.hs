module Main where

import Convolution (cacf)
import MLS (mls)

main :: IO ()
main = print (cacf s s)
  where
    s = map bool2int $ mls [True, False,False, True] [False, False,True]

bool2int :: Num p => Bool -> p
bool2int True = 1
bool2int False = 0
