import Test.QuickCheck
import Data.Hashable
import Data.HashMap as HM
import Data.List (foldl')
import Data.Maybe (isNothing)

instance (Eq k, Hashable k, Arbitrary k, Arbitrary v) => Arbitrary (HashMap k v) where
    arbitrary = do
        list <- arbitrary
        return $ fromList (fromIntegral.hash) list

prop_insert :: (Eq k, Hashable k, Eq v) => k -> v -> HashMap k v -> Bool
prop_insert k v hm =
    let hm' = insert k v hm
        in    member k hm'
           && not (notMember k hm')
           && HM.lookup k hm' == Just v
           && hm' ! k == v

prop_delete :: (Eq k, Hashable k) => k -> HashMap k v -> Bool
prop_delete k hm =
    let hm' = delete k hm
        in    not (member k hm')
           && notMember k hm'
           && isNothing (HM.lookup k hm')

prop_fromList :: (Eq k, Hashable k, Eq v) => [(k, v)] -> Bool
prop_fromList list =
    let hm = fromList (fromIntegral.hash) list
        hm' = foldl' (\hm (k,v) -> insert k v hm) (empty (fromIntegral.hash)) list
        in toList hm == toList hm'


main = do
    quickCheck (prop_insert :: Int -> Int -> HashMap Int Int -> Bool)
    quickCheck (prop_delete :: Int -> HashMap Int Int -> Bool)
    quickCheck (prop_fromList :: [(Int, Int)] -> Bool)
