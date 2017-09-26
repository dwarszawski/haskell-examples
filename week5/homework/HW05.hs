{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module HW05 where

import           Data.ByteString.Lazy (ByteString)
import           Data.Map.Strict      (Map)
import           System.Environment   (getArgs)

import           Data.Bits            (xor)
import qualified Data.ByteString.Lazy as BS
import qualified Data.Map.Strict      as Map
import System.FilePath.Posix (addExtension)
import qualified Data.ByteString.Internal as BS (c2w, w2c)
import qualified Data.List (sortBy)
import           Control.Arrow (second)

import           Parser

-- Exercise 1 -----------------------------------------
getSecret :: FilePath -> FilePath -> IO ByteString
getSecret orig modif = do
  original <- BS.readFile orig
  modified <- BS.readFile modif
  let decrypted = BS.filter (/=0) $ decrypt original modified
  return decrypted


decrypt :: ByteString -> ByteString -> ByteString
decrypt x y =  BS.pack $ BS.zipWith xor x y

-- Exercise 2 -----------------------------------------


decryptWithKey :: ByteString -> FilePath -> IO ()
decryptWithKey key dir = do
  encrypted <- BS.readFile (addExtension dir "enc")
  let decrypted = decrypt (BS.cycle key) encrypted
  BS.writeFile dir decrypted


-- Exercise 3 -----------------------------------------

parseFile :: FromJSON a => FilePath -> IO (Maybe a)
parseFile path = decode <$> BS.readFile path

-- Exercise 4 -----------------------------------------

getBadTs :: FilePath -> FilePath -> IO (Maybe [Transaction])
getBadTs vpath tpath = do
  Just vs <- parseFile vpath :: IO (Maybe[TId])
  ts <- parseFile tpath :: IO (Maybe[Transaction])
  let contains (Transaction {tid = x}) = elem x vs
  return $ filter contains <$> ts

-- Exercise 5 -----------------------------------------
type Entry = (String, Integer)

getFlow :: [Transaction] -> Map String Integer
getFlow ts =  Map.fromList $ concat $ map (toTuple) ts
  where toTuple :: Transaction -> [(Entry)]
        toTuple (Transaction {from = x , to = y,  amount = z}) = [(x, (-1) * z), (y, z)]
-- Exercise 6 ----------------------1-------------------

getCriminal :: Map String Integer -> String
getCriminal ts = fst $ head $ Data.List.sortBy compareTuple $ Map.toList ts

compareTuple :: Entry -> Entry -> Ordering
compareTuple (_, v1) (_, v2) | v1 > v2 = LT
                             | v2 > v1 = GT
                             | v1 == v2 = EQ
--
-- badGuy :: IO (String)
-- badGuy = do
--   Just ts <- getBadTs "victims.json" "transactions.json"
--   let fl = getCriminal $ getFlow ts
--   return $ fl


-- Exercise 7 -----------------------------------------

undoTs :: Map String Integer -> [TId] -> [Transaction]
undoTs ts ids = map (\(((pe, am1),(py, am2)), tid) -> Transaction{from = pe , to = py, amount = min (abs am1) (abs am2), tid = tid}) (zip (zip payee payers) ids)
  where
    payee = Data.List.sortBy compareTuple $ filter  (\(_, b)-> b > 0) $ Map.toList ts
    payers = Data.List.sortBy compareTuple $ map (second abs) $ filter  (\(_, b)-> b < 0) $ Map.toList ts


-- Exercise 8 -----------------------------------------

writeJSON :: ToJSON a => FilePath -> a -> IO ()
writeJSON path ts = BS.writeFile path (encode ts)

-- Exercise 9 -----------------------------------------

doEverything :: FilePath -> FilePath -> FilePath -> FilePath -> FilePath
             -> FilePath -> IO String
doEverything dog1 dog2 trans vict fids out = do
  key <- getSecret dog1 dog2
  decryptWithKey key vict
  mts <- getBadTs vict trans
  case mts of
    Nothing -> error "No Transactions"
    Just ts -> do
      mids <- parseFile fids
      case mids of
        Nothing  -> error "No ids"
        Just ids -> do
          let flow = getFlow ts
          writeJSON out (undoTs flow ids)
          return (getCriminal flow)

main :: IO ()
main = do
  args <- getArgs
  crim <-
    case args of
      dog1:dog2:trans:vict:ids:out:_ ->
          doEverything dog1 dog2 trans vict ids out
      _ -> doEverything "dog-original.jpg"
                        "dog.jpg"
                        "transactions.json"
                        "victims.json"
                        "new-ids.json"
                        "new-transactions.json"
  putStrLn crim
