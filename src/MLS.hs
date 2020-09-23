module MLS () where

import Control.Monad.State.Lazy
  ( MonadState (get, put),
    State,
    evalState,
    replicateM,
  )
import Data.Complex (Complex (..))

j :: RealFloat a => Complex a
j = 0 :+ 1

xor :: Bool -> Bool -> Bool
xor True a = not a
xor False a = a

tick :: [Bool] -> State [Bool] Bool
tick poly = do
  let coefficients = tail poly -- polynomial member with power of 0 is not a part of register
  regVals <- get
  let result = foldl xor False [y | (x, y) <- zip coefficients regVals, x]
  put $ result : init regVals
  return $! last regVals

mls :: [Bool] -> [Bool] -> [Bool]
mls polynome register =
  if length polynome - 1 == length register
    then evalState (replicateM n nextState) register
    else error "Wrong length of init register"
  where
    n = (length polynome - 1) ^ 2 - 1
    nextState = tick polynome
