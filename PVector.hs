module PVector () where

import PVector.Internal as I
import Data.Array
import Control.Exception

data PVector e = PV {
      cnt :: Int
    , root :: Node e
    , tailBuffer :: Array Int e
    }

instance (Show e) => Show (PVector e) where
    show pv = show $ map ((tailBuffer pv) !) [0..(cnt pv)-1]

data Node e = EmptyNode | Array Int e

newEmpty :: PVector e
newEmpty = PV 0 EmptyNode $ array (0, 31) []

nth :: PVector e -> Int -> e
pv `nth` ix | ix >= cnt pv = throw $ IndexOutOfBounds ""
            | ix >= 32     = throw $ IndexOutOfBounds "TODO"
            | otherwise    = (tailBuffer pv) ! ix

conj :: PVector e -> e -> PVector e
conj pv el | cnt pv >= 32 = throw $ IndexOutOfBounds "TODO"
           | otherwise    = PV ((cnt pv)+1) (root pv) (tailBuffer pv // [(cnt pv, el)])

assoc :: PVector e -> Int -> e -> PVector e
assoc pv ix el | ix >= cnt pv = throw $ IndexOutOfBounds ""
               | ix >= 32     = throw $ IndexOutOfBounds "TODO"
               | otherwise    = PV (cnt pv) (root pv) (tailBuffer pv // [(ix, el)])

main = putStrLn "ok."
