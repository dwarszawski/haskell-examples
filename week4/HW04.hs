{-# OPTIONS_GHC -Wall #-}
module HW04 where

newtype Poly a = P [a]

-- Exercise 1 -----------------------------------------

x :: Num a => Poly a
x = P [0,1]

-- Exercise 2 ----------------------------------------

instance (Num a, Eq a) => Eq (Poly a) where
    (==) (P a) (P b) = clean a ==  clean b
      where clean :: (Num a, Eq a) => [a] -> [a]
            clean =  dropWhile (==0) . reverse

-- Exercise 3 -----------------------------------------

instance (Num a, Eq a, Show a) => Show (Poly a) where
    show (P b) = format (reverse b) (length b -1)
      where format :: (Num a, Eq a, Show a) => [a] -> Int -> String
            format [h] _
              | h /=0 = show h
              | h ==0 = ""
            format (h:hs) ex
              | h == 1  = "x^" ++ show ex ++ "+" ++ format hs (ex -1)
              | h /=0 = show h ++ "x^" ++ show ex  ++ "+" ++  format hs (ex-1)
              | h ==0 = format hs (ex -1)
            format [] _ = ""
-- Exercise 4 -----------------------------------------

plus :: Num a => Poly a -> Poly a -> Poly a
plus (P a) (P b) =  P(merge a b)
  where merge :: Num a => [a] -> [a] -> [a]
        merge [] [] = []
        merge a b = map (uncurry(+)) $ pairs a b

pairs :: Num a => [a] -> [a] -> [(a,a)]
pairs a b
  | (length a) > (length b) = zip a (fillZeros a b)
  | otherwise = zip b (fillZeros b a)


fillZeros :: Num a => [a] -> [a] -> [a]
fillZeros t1 t2 = foldr (.) id (replicate (diffSize (t1) (t2)) (++[0])) t2

diffSize :: Num a => [a] -> [a] -> Int
diffSize a b = abs ((length a) - (length b))

-- Exercise 5 -----------------------------------------

times :: Num a => Poly a -> Poly a -> Poly a
times (P a) (P b) = permutate a b 0
  where permutate :: Num a =>  [a] -> [a] -> Int -> Poly a
        permutate (h:hs) b x = P (map (*h) $ foldr (.) id (replicate x ([0]++)) b) `plus`  permutate hs b (x + 1)
        permutate [] _ _ = P[]
-- Exercise 6 -----------------------------------------

instance Num a => Num (Poly a) where
    (+) = plus
    (*) = times
    negate (P a) = P $ map negate a
    fromInteger a = P [fromIntegral a]
    -- No meaningful definitions exist
    abs    = undefined
    signum = undefined

-- Exercise 7 -----------------------------------------

applyP :: Num a => Poly a -> a -> a
applyP (P tmp) x = foldr (\y acc -> y + x * acc) 0 tmp

-- Exercise 8 -----------------------------------------

class Num a => Differentiable a where
    deriv  :: a -> a
    nderiv :: Int -> a -> a
    nderiv n a = nderiv (n-1) (deriv a)

-- Exercise 9 -----------------------------------------

instance Num a => Differentiable (Poly a) where
      deriv (P coefs) = P (tail $ derivCoefs coefs)
              where
                  derivCoefs _ [] = []
                  derivCoefs pow (x:xs) = (x * pow) : (derivCoefs (pow + 1) xs)
             --       derivCoefs a = zipWith (*) [0..] a
