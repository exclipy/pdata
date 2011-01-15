import Test.QuickCheck
import Test.QuickCheck.Batch
import Data.Hashable
import Data.HashMap as HM
import Data.Int
import Data.List (foldl', sort)
import Data.Maybe (isNothing)

prop_insert :: (Eq k, Hashable k, Eq v) => (k -> Int32) -> k -> v -> [(k, v)] -> Bool
prop_insert hashFn k v lm =
    let hm  = fromList hashFn lm
        hm' = insert k v hm
        in    member k hm'
           && not (notMember k hm')
           && HM.lookup k hm' == Just v
           && hm' ! k == v

prop_delete :: (Eq k, Hashable k) => (k -> Int32) -> k -> [(k, v)] -> Bool
prop_delete hashFn k lm =
    let hm  = fromList hashFn lm
        hm' = delete k hm
        in    not (member k hm')
           && notMember k hm'
           && isNothing (HM.lookup k hm')

prop_fromList :: (Eq k, Hashable k, Ord k, Eq v, Ord v) => (k -> Int32) -> [(k, v)] -> Bool
prop_fromList hashFn list =
    let hm = fromList hashFn list
        hm' = foldl' (\hm (k,v) -> insert k v hm) (empty hashFn) list
        in sort (toList hm) == sort (toList hm')

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
    ]
