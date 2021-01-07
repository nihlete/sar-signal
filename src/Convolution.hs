module Convolution (cacf, iacf, convolution) where

barker11 :: [Int]
barker11 = [1, 1, 1, -1, -1, -1, 1, -1, -1, 1, -1]

barker4 :: [Int]
barker4 = [1, -1, 1, 1]

convolution :: Num a => [a] -> [a] -> a
convolution a b = sum $ zipWith (*) a b

shift :: [a] -> [a]
shift [] = []
shift (x : xs) = xs ++ [x]

-- todo: add check a == b
cacf :: Num a => [a] -> [a] -> [a]
cacf a b = map (convolution a) $ take n $ iterate shift b
  where
    n = length a

iacf :: Num a => [a] -> [a] -> [a]
iacf a b = cacf a' b'
  where
    a' = a ++ replicate (length b - 1) 0
    b' = replicate (length a - 1) 0 ++ b