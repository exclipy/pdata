module PVector () where

import PVector.Internal as I
import Data.Array
import Data.Bits hiding (shift)
import Control.Exception
import Prelude hiding (tail)

data PVector e = PV {
      cnt :: Int
    , shift :: Int
    , root :: Node e
    , tail :: Array Int e
    }

instance (Show e) => Show (PVector e) where
    show pv = show $ map ((tail pv) !) [0..(cnt pv)-1]

data Node e = BodyNode (Array Int (Node e)) |
              TailNode (Array Int e)

emptyNode :: Node e
emptyNode = Node $ array (0, 31) []

empty :: PVector e
empty = PV 0 5 emptyNode $ array (0, -1) []

index :: PVector e -> Int -> e
pv `index` ix | ix >= cnt pv = throw $ IndexOutOfBounds ""
              | ix >= 32     = throw $ IndexOutOfBounds "TODO"
              | otherwise    = (tail pv) ! ix

(|>) :: PVector e -> e -> PVector e
pv |> el =
    let c = cnt pv
        s = shift pv
        r = root pv
        t = tail pv
        tailIx = c - tailoff c
        in if tailIx < 32
              then let newTail = listArray (0, tailIx) $ elems t ++ [el]
                       in PV (c+1) s r newTail
              else let overflow = ((c `shiftR` s) > (1 `shiftL` s))
                       newShift = if overflow
                                     then s
                                     else s + 5
                       newRoot = if overflow
                                    then undefined
                                    else pushTail c s r t
                       newTail = listArray (0, 0) [el]
                       in PV (c+1) newShift newRoot newTail

    where pushTail :: Int -> Int -> Node e -> Array Int e -> Node e
          pushTail cnt level (Node parent) tail =
              let subIx = ((cnt-1) `shiftR` level) .&. 0x1f
                  in if level == 5
                        then Node $ parent // [(subIx, tail)]
                        else undefined


update :: Int -> e -> PVector e -> PVector e
update ix el pv | ix >= cnt pv = throw $ IndexOutOfBounds ""
                | ix >= 32     = throw $ IndexOutOfBounds "TODO"
                | otherwise    = PV (cnt pv) (shift pv) (root pv) (tail pv // [(ix, el)])

tailoff :: Int -> Int
tailoff count = if count < 32
                   then 0
                   else (count-1) .&. (complement 0x1f)

main = do
    let a = (empty :: PVector Int)
        b = a |> 1
        c = foldl (|>) empty [1..32]
        d = foldl (|>) empty [1..33]
        in do print a
              print b
              print c
              print d
