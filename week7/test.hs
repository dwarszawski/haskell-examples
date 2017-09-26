module Monads where

import qualified Data.Foldable as F

  --it is the sam as bindMaybe but first argument inlined in to the function definition of in separate case
--instance Monad Maybe where
--    return = Just
--    Nothing >>= _ = Nothing
--    Just x >>= k = k x

data Tree a = Node (Tree a) (Tree a) a
               | Empty deriving (Show)

zipTree1 :: (a -> b -> c) -> Tree a -> Tree b -> Maybe(Tree c)
zipTree1 _ Node{} Empty = Nothing
zipTree1 _ Empty Node{} = Nothing
zipTree1 _ Empty Empty = Nothing
zipTree1 f (Node l1 r1 x) (Node l2 r2 y) = do
  l <- zipTree1 f l1 l2
  r <- zipTree1 f r1 r2
  return $ Node l r (f x y)
--zipTree1 f (Node l1 r1 x) (Node l2 r2 y) = zipTree1 f l1 l2 >>= \l ->
--                                                      zipTree1 f r1 r2 >>= \r ->
--                                                          Just (Node l r (f x y))


--    case zipTree1 f l1 l2 of
--      Nothing -> Nothing
--      Just l -> case zipTree1 f r1 r2 of
--        Nothing -> Nothing
--        Just r -> Just $ Node l r (f x y)


bindMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
bindMaybe ma f = case ma of
                  Nothing -> Nothing
                  Just x -> f x

main :: IO()
main = do
  a <- (++) <$> getLine <*> getLine
  putStrLn $ "Hello World to " ++ a


data Profession = Scala | Java | Haskell
data Level = Junior | Mid | Senior
data Programmer = Programmer Profession Level

newtype CharList = CharList {getCharList :: [Char]} deriving (Eq, Show)

data ZipList a = ZipList{getZipList :: [a]}
newtype ZipList1 a = ZipList1{getZipList1 :: [a]}

data CoolBool1 = CoolBool1{getBool :: Bool}
newtype CoolBool2 = CoolBool2{getBool2 :: Bool}

getCoolBool :: CoolBool2 -> String
getCoolBool (CoolBool2 _) = "hello"

lengthCompare :: String -> String -> Ordering
lengthCompare a b = let x = length a `compare` length b
                        y = a `compare` b
                      in if x == EQ then y else x

newtype First a = First {getFirst :: Maybe a} deriving (Ord, Eq, Read, Show)

instance Monoid (First a) where
   mempty = First Nothing
   First(Just x) `mappend` _ = First(Just x)
   First Nothing `mappend` x = x

instance F.Foldable Tree where
  foldMap _ Empty = mempty
  foldMap f (Node x l r) = F.foldMap f l `mappend`
                           F.foldMap f x `mappend`
                           f r
