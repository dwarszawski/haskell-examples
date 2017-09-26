{-# OPTIONS_GHC -Wall #-}
module HW02 where

-- Mastermind -----------------------------------------

-- A peg can be one of six colors
data Peg = Red | Green | Blue | Yellow | Orange | Purple
         deriving (Show, Eq, Ord)

-- A code is defined to simply be a list of Pegs
type Code = [Peg]

-- A move is constructed using a Code and two integers; the number of
-- exact matches and the number of regular matches
data Move = Move Code Int Int
          deriving (Show, Eq)

-- List containing all of the different Pegs
colors :: [Peg]
colors = [Red, Green, Blue, Yellow, Orange, Purple]

-- Exercise 1 -----------------------------------------

count :: (a -> Bool) -> [a] -> Int
count p xs = length $ filter p xs

-- Get the number of exact matches between the actual code and the guess
exactMatches :: Code -> Code -> Int
exactMatches c c' = (count eq) $ zip c c'
  where eq = uncurry (==)
--exactMatches a b = exactMatch 0 a b
--  where exactMatch :: Int -> Code -> Code -> Int
--        exactMatch acc [] [] = acc
--       exactMatch acc (a:as) (b:bs)
--        | a == b = exactMatch (acc + 1) as bs
--         | otherwise = exactMatch acc as bs

-- Exercise 2 -----------------------------------------

-- For each peg in xs, count how many times is occurs in ys
countColors :: Code -> [Int]
countColors code = [count (c ==) code | c <- colors]
--countColors x =  map (\a -> matched a x 0) colors
--  where matched :: Peg -> Code -> Int -> Int
--        matched _ [] acc = acc
--        matched a (b:bs) acc
--         | a == b = matched a bs acc+1
--         | otherwise = matched a bs acc

-- Count number of matches between the actual code and the guess
matches :: Code -> Code -> Int
matches c c' = sum $ map (uncurry min) $ zip (countColors c) (countColors c')
--matches c g = sum $ intersect (countColors g) (countColors c)
--   where intersect :: [Int] -> [Int] -> [Int]
--         intersect (a:as)(b:bs) = countSum a b : intersect as bs
--         intersect [] [] = [0]


--countSum :: Int -> Int -> Int
--countSum a b
--  | a > 0 && b > 0 && a == b = a
--  | a > 0 && b > 0 && a > b = b
--  | a > 0 && b > 0 && a < b = a
--  | otherwise = 0


-- Exercise 3 -----------------------------------------

-- Construct a Move from a guess given the actual code
getMove :: Code -> Code -> Move
getMove c c' = Move c' m (n - m)
 where m = exactMatches c c'
       n = matches c c'
--getMove a b =  Move b  (exactMatches a b)  ((matches a b) - (exactMatches a b))

-- Exercise 4 -----------------------------------------

isConsistent :: Move -> Code -> Bool
isConsistent (Move xs m n) ys = m == m' && n == n'
  where Move _ m' n' = getMove xs ys

--isConsistent (Move a b c) d = exactMatches a d == b && ((matches a d) - (exactMatches a d)) == c

-- Exercise 5 -----------------------------------------

filterCodes :: Move -> [Code] -> [Code]
filterCodes m cs = filter (isConsistent m) cs

-- Exercise 6 -----------------------------------------

allCodes :: Int -> [Code]
allCodes 0 = [[]]
allCodes n = [c : code | c <- colors , code <- allCodes (n-1)]

-- Exercise 7 -----------------------------------------

solve :: Code -> [Move]
solve s = choose s allCodes(6)

choose :: Code [Code] -> [Move]
choose _ [] = []
choose c (x:xs) =
  if (x == c) then [m] else m : choose(c $ filterCodes m xs)
  where m = getMove c x

-- Bonus ----------------------------------------------

fiveGuess :: Code -> [Move]
fiveGuess = undefined
