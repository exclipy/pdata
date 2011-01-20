import Test.QuickCheck
import Test.QuickCheck.Batch
import Data.Hashable
import Data.HamtMap as HM
import Data.Int
import Data.List (foldl', sort)
import Data.Maybe (isNothing)
import Prelude as P

newtype Inty = Inty Int deriving (Eq, Show, Ord)

instance Hashable Inty where
    hash (Inty i) = fromIntegral (hash i `mod` 2)

instance Arbitrary Inty where
    arbitrary = do
        x <- arbitrary
        return $ Inty x

ldelete :: (Eq k) => k -> [(k, v)] -> [(k, v)]
ldelete _ [] = []
ldelete k ((k', v'):xs) | k' == k   = ldelete k xs
                        | otherwise = (k', v') : ldelete k xs

lset :: (Eq k) => k -> v -> [(k, v)] -> [(k, v)]
lset k v [] = []
lset k v ((k', v'):xs) | k' == k   = (k, v) : lset k v xs
                       | otherwise = (k', v') : lset k v xs

prop_insert :: (Eq k, Hashable k, Eq v) => k -> v -> [(k, v)] -> Bool
prop_insert k v lm =
    let hm  = fromList lm
        hm' = insert k v hm
        in    member k hm'
           && not (notMember k hm')
           && HM.lookup k hm' == Just v
           && hm' ! k == v
           && (if member k hm
                  then toList hm == lset k (hm ! k) (toList hm')
                  else toList hm == ldelete k (toList hm')
                  )

prop_delete :: (Eq k, Hashable k, Eq v) => k -> [(k, v)] -> Bool
prop_delete k lm =
    let hm  = fromList lm
        hm' = delete k hm
        in    not (member k hm')
           && notMember k hm'
           && isNothing (HM.lookup k hm')
           && (if member k hm
                  then toList hm' == ldelete k (toList hm)
                  else toList hm' == toList hm
                  )

prop_fromList :: (Eq k, Hashable k, Ord k, Eq v, Ord v) => [(k, v)] -> Bool
prop_fromList lm =
    let hm = fromList lm
        hm' = foldl' (\hm (k,v) -> insert k v hm) empty lm
        in sort (toList hm) == sort (toList hm')

prop_toList :: (Eq k, Hashable k, Eq v) => [(k, v)] -> Bool
prop_toList lm =
    let hm = fromList lm
        lm' = toList hm
        ks = keys hm
        ks' = P.map fst lm'
        els = elems hm
        els' = P.map snd lm'
        els'' = P.map (\k -> hm ! k) ks
        in    ks == ks'
           && els == els'
           && els == els''

prop_map :: (Eq k, Hashable k, Integral v) => [(k, v)] -> Bool
prop_map lm =
    let hm = fromList lm
        lm' = toList hm
        in toList (HM.map (*2) hm) == P.map (\(x,y) -> (x, y*2)) lm'

prop_filter :: (Eq k, Hashable k, Integral v) => [(k, v)] -> Bool
prop_filter lm =
    let hm = fromList lm
        lm' = toList hm
        in toList (HM.filter even hm) == P.filter (\(x,y) -> even y) lm'


options = TestOptions
      { no_of_tests         = 200
      , length_of_tests     = 2 -- seconds
      , debug_tests         = False }

main = runTests "tests" options
    [ run (prop_insert :: Int -> Int -> [(Int,Int)] -> Bool)
    , run (prop_insert :: Inty -> Int -> [(Inty,Int)] -> Bool)
    , run (prop_delete :: Int -> [(Int, Int)] -> Bool)
    , run (prop_delete :: Inty -> [(Inty, Int)] -> Bool)
    , run (prop_fromList :: [(Int, Int)] -> Bool)
    , run (prop_fromList :: [(Inty, Int)] -> Bool)
    , run (prop_toList :: [(Int, Int)] -> Bool)
    , run (prop_toList :: [(Inty, Int)] -> Bool)
    , run (prop_map :: [(Int, Int)] -> Bool)
    , run (prop_map :: [(Inty, Int)] -> Bool)
    , run (prop_filter :: [(Int, Int)] -> Bool)
    , run (prop_filter :: [(Inty, Int)] -> Bool)
    ]
