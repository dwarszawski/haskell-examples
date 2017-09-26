{-# OPTIONS_GHC -Wall #-}
module HW06 where

import Data.List
import Data.Functor

-- Exercise 1 -----------------------------------------

fib :: Integer -> Integer
fib n
 | n == 0 = 1
 | n == 1 = 1
 | otherwise = fib (n -1) + fib (n+1)

fibs1 :: [Integer]
fibs1 = map fib [0..]

-- Exercise 2 -----------------------------------------

fibs2 :: [Integer]
fibs2 = 1 : 1 : zipWith (+) fibs2 (tail fibs2)

-- Exercise 3 -----------------------------------------

data Stream a = Cons a (Stream a)

-- Show instance prints the first 20 elements followed by ellipsis
instance Show a => Show (Stream a) where
    show s = "[" ++ intercalate ", " (map show $ take 10 $ streamToList s)
             ++ ",..."

streamToList :: Stream a -> [a]
streamToList (Cons v b)  = v : streamToList b

-- Exercise 4 -----------------------------------------

instance Functor Stream where
    fmap f (Cons v b) = Cons (f v) (fmap f b)

-- Exercise 5 -----------------------------------------

sRepeat :: a -> Stream a
sRepeat a = Cons a (sRepeat a)

sIterate :: (a -> a) -> a -> Stream a
sIterate f a = Cons a (sIterate f (f a))

sInterleave :: Stream a -> Stream a -> Stream a
sInterleave (Cons v s1) s2 = Cons v (sInterleave s2 s1)

sTake :: Int -> Stream a -> [a]
sTake n (Cons v s)
  | n == 0 = []
  | otherwise = v : sTake (n-1) s

-- Exercise 6 -----------------------------------------

nats :: Stream Integer
nats = sIterate (+1) 1

ruler :: Stream Integer
ruler = foldr1 sInterleave(map sRepeat [0..])

-- Exercise 7 -----------------------------------------

-- | Implementation of C rand
rand :: Int -> Stream Int
rand = sIterate((`mod` 2147483648) . (12345 + ) . (1103515245 * ))
-- Exercise 8 -----------------------------------------

{- Total Memory in use: 110 000 MB -}
minMaxSlow :: [Int] -> Maybe (Int, Int)
minMaxSlow [] = Nothing   -- no min or max if there are no elements
minMaxSlow xs = Just (minimum xs, maximum xs)

-- Exercise 9 -----------------------------------------

{- Total Memory in use: 3656 KB -}
minMax :: [Int] -> Maybe (Int, Int)
minMax [] = Nothing
minMax (x:xs) = go x x xs
  where go a b [] = Just (a, b)
        go a b (x:xs)
         | x < a = go x b xs
         | x > b = go a x xs
         | otherwise = go a b xs

main :: IO ()
main = print $ minMax $ sTake 1000000 $ rand 7666532

-- Exercise 10 ----------------------------------------

fastFib :: Int -> Integer
fastFib = undefined
