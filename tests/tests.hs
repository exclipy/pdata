import Test.QuickCheck
import Test.QuickCheck.Batch
import Data.Hashable
import Data.HashMap as HM
import Data.Int
import Data.List (foldl', sort)
import Data.Maybe (isNothing)
import Prelude as P

ldelete :: (Eq k) => k -> [(k, v)] -> [(k, v)]
ldelete _ [] = []
ldelete k ((k', v'):xs) | k' == k   = ldelete k xs
                        | otherwise = (k', v') : ldelete k xs

lset :: (Eq k) => k -> v -> [(k, v)] -> [(k, v)]
lset k v [] = []
lset k v ((k', v'):xs) | k' == k   = (k, v) : lset k v xs
                       | otherwise = (k', v') : lset k v xs

prop_insert :: (Eq k, Hashable k, Eq v) => (k -> Int32) -> k -> v -> [(k, v)] -> Bool
prop_insert hashFn k v lm =
    let hm  = fromList hashFn lm
        hm' = insert k v hm
        in    member k hm'
           && not (notMember k hm')
           && HM.lookup k hm' == Just v
           && hm' ! k == v
           && (if member k hm
                  then toList hm == lset k (hm ! k) (toList hm')
                  else toList hm == ldelete k (toList hm')
                  )

prop_delete :: (Eq k, Hashable k, Eq v) => (k -> Int32) -> k -> [(k, v)] -> Bool
prop_delete hashFn k lm =
    let hm  = fromList hashFn lm
        hm' = delete k hm
        in    not (member k hm')
           && notMember k hm'
           && isNothing (HM.lookup k hm')
           && (if member k hm
                  then toList hm' == ldelete k (toList hm)
                  else toList hm' == toList hm
                  )

prop_fromList :: (Eq k, Hashable k, Ord k, Eq v, Ord v) => (k -> Int32) -> [(k, v)] -> Bool
prop_fromList hashFn lm =
    let hm = fromList hashFn lm
        hm' = foldl' (\hm (k,v) -> insert k v hm) (empty hashFn) lm
        in sort (toList hm) == sort (toList hm')

prop_toList :: (Eq k, Hashable k, Ord k, Eq v, Ord v) => (k -> Int32) -> [(k, v)] -> Bool
prop_toList hashFn lm =
    let hm = fromList hashFn lm
        lm' = toList hm
        ks = keys hm
        ks' = P.map fst lm'
        els = elems hm
        els' = P.map snd lm'
        els'' = P.map (\k -> hm ! k) ks
        in    ks == ks'
           && els == els'
           && els == els''


options = TestOptions
      { no_of_tests         = 200
      , length_of_tests     = 2 -- seconds
      , debug_tests         = False }

main = runTests "tests" options
    [ run (prop_insert fromIntegral :: Int -> Int -> [(Int,Int)] -> Bool)
    , run (prop_insert (fromIntegral.(`mod` 2)) :: Int -> Int -> [(Int,Int)] -> Bool)
    , run (prop_delete fromIntegral :: Int -> [(Int, Int)] -> Bool)
    , run (prop_delete (fromIntegral.(`mod` 2)) :: Int -> [(Int,Int)] -> Bool)
    , run (prop_fromList fromIntegral :: [(Int, Int)] -> Bool)
    , run (prop_fromList (fromIntegral.(`mod` 2)) :: [(Int, Int)] -> Bool)
    , run (prop_toList fromIntegral :: [(Int, Int)] -> Bool)
    , run (prop_toList (fromIntegral.(`mod` 2)) :: [(Int, Int)] -> Bool)
    ]
