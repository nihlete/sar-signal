module Correlation (cacf, iacf, correlation) where

correlation :: Num a => [a] -> [a] -> a
correlation a b = sum $ zipWith (*) a b

shift :: [a] -> [a]
shift [] = []
shift (x : xs) = xs ++ [x]

-- todo: add check a == b
cacf :: Num a => [a] -> [a] -> [a]
cacf a b = map (correlation a) $ take n $ iterate shift b
  where
    n = length a

iacf :: Num a => [a] -> [a] -> [a]
iacf a b = cacf a' b'
  where
    a' = a ++ replicate (length b - 1) 0
    b' = replicate (length a - 1) 0 ++ b