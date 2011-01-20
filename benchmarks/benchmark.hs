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
import qualified Data.HamtMap as HM
import Data.List (foldl')
import Data.Maybe (fromMaybe)
import Prelude hiding (lookup)
import System.Random (mkStdGen, randomRs)

instance NFData BS.ByteString

main :: IO ()
main = do
    let hmbs = HM.fromList elemsBS :: HM.HamtMap BS.ByteString Int
    defaultMainWith defaultConfig
        (liftIO . evaluate $ rnf [hmbs])
        [ bench "fromList" $ nf HM.fromList elemsBS
        , bench "lookup" $ nf (lookup keysBS) hmbs
        , bench "insert" $ nf (insert elemsBS) HM.empty
        , bench "delete" $ nf (delete keysBS) hmbs
        ]
  where
    n :: Int
    n = 2^(12 :: Int)

    elemsBS = zip keysBS [1..n]
    keysBS  = rnd 8 n

lookup :: (Eq k, Hashable k) => [k] -> HM.HamtMap k Int -> Int
lookup xs m = foldl' (\z k -> fromMaybe z (HM.lookup k m)) 0 xs

insert :: (Eq k, Hashable k) => [(k, Int)] -> HM.HamtMap k Int -> HM.HamtMap k Int
insert xs m0 = foldl' (\m (k, v) -> HM.insert k v m) m0 xs

delete :: (Eq k, Hashable k) => [k] -> HM.HamtMap k Int -> HM.HamtMap k Int
delete xs m0 = foldl' (\m k -> HM.delete k m) m0 xs

-- | Generate a number of fixed length strings where the content of
-- the strings are letters in random order.
rnd :: Int  -- ^ Length of each string
    -> Int  -- ^ Number of strings
    -> [BS.ByteString]
rnd strlen num = map C.pack $ take num $ split $ randomRs ('a', 'z') $ mkStdGen 1234
  where
    split cs = case splitAt strlen cs of (str, cs') -> str : split cs'
