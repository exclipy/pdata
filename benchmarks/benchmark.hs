{-# LANGUAGE GADTs #-}

module Main where

import Control.DeepSeq
import Control.Exception (evaluate)
import Control.Monad.Trans (liftIO)
import Criterion.Config
import Criterion.Main
import Data.Hashable (Hashable(hash))
import Data.Int (Int32)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C
import qualified Data.PHashMap as PM
import Data.List (foldl')
import Data.Maybe (fromMaybe)
import Prelude hiding (lookup)
import System.Random (mkStdGen, randomRs)

instance NFData BS.ByteString

hashBS :: BS.ByteString -> Int32
hashBS = fromIntegral . hash

main :: IO ()
main = do
    let hmbs = PM.fromList hashBS elemsBS :: PM.PHashMap BS.ByteString Int
    defaultMainWith defaultConfig
        (liftIO . evaluate $ rnf [hmbs])
        [ bench "lookup" $ nf (lookup keysBS) hmbs
        , bench "insert" $ nf (insert elemsBS) (PM.empty hashBS)
        , bench "delete" $ nf (delete keysBS) hmbs
        ]
  where
    n :: Int
    n = 2^(12 :: Int)

    elemsBS = zip keysBS [1..n]
    keysBS  = rnd 8 n

lookup :: Eq k => [k] -> PM.PHashMap k Int -> Int
lookup xs m = foldl' (\z k -> fromMaybe z (PM.lookup k m)) 0 xs

insert :: Eq k => [(k, Int)] -> PM.PHashMap k Int -> PM.PHashMap k Int
insert xs m0 = foldl' (\m (k, v) -> PM.insert k v m) m0 xs

delete :: Eq k => [k] -> PM.PHashMap k Int -> PM.PHashMap k Int
delete xs m0 = foldl' (\m k -> PM.delete k m) m0 xs

-- | Generate a number of fixed length strings where the content of
-- the strings are letters in random order.
rnd :: Int  -- ^ Length of each string
    -> Int  -- ^ Number of strings
    -> [BS.ByteString]
rnd strlen num = map C.pack $ take num $ split $ randomRs ('a', 'z') $ mkStdGen 1234
  where
    split cs = case splitAt strlen cs of (str, cs') -> str : split cs'
