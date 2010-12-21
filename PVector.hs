module PVector (PVector, index, (|>), toList, empty) where

import Data.Array hiding (index)
import Data.Bits hiding (shift)
import Control.Exception
import Prelude hiding (tail)

shiftStep = 5
chunk = 2^shiftStep
mask = pred chunk

data PVector e = PV {
      cnt :: Int
    , shift :: Int
    , root :: Node e
    , tail :: Array Int e
    }

instance (Show e) => Show (PVector e) where
    show = show.toList

data Node e = BodyNode (Array Int (Node e)) |
              LeafNode (Array Int e)

emptyNode :: Node e
emptyNode = BodyNode $ array (0, -1) []

empty :: PVector e
empty = PV 0 shiftStep emptyNode $ array (0, -1) []

index :: PVector e -> Int -> e
(PV c s r t) `index` ix | ix >= c || ix < 0 = throw $ IndexOutOfBounds ""
                        | ix >= tailOff c   = t ! (ix - tailOff c)
                        | otherwise         = lookup r s ix
    where lookup :: Node e -> Int -> Int -> e
          lookup node level ix = let subIx = (ix `shiftR` level) .&. mask
                                     in case node of
                                             BodyNode a -> lookup (a ! subIx) (level-shiftStep) ix
                                             LeafNode a -> a ! subIx

(|>) :: PVector e -> e -> PVector e
(PV c s r t) |> el =
    let tailIx = c - tailOff c
        in if tailIx < chunk
              then let newTail = listArray (0, tailIx) $ elems t ++ [el]
                       in PV (c+1) s r newTail
              else let overflow = ((c `shiftR` shiftStep) > (1 `shiftL` s))
                       newShift = if overflow
                                     then s + shiftStep
                                     else s
                       newRoot = if overflow
                                    then BodyNode $ listArray (0, 1) [r, newPath s t]
                                    else pushTail c s r t
                       newTail = listArray (0, 0) [el]
                       in PV (c+1) newShift newRoot newTail

pushTail :: Int -> Int -> Node e -> Array Int e -> Node e
pushTail cnt level (BodyNode parent) tail =
    let subIx = ((cnt-1) `shiftR` level) .&. mask
        in if level == shiftStep
              then BodyNode $ listArray (0, subIx) $ elems parent ++ [LeafNode tail]
              else if subIx > snd (bounds parent)
                      then BodyNode (listArray (0, subIx) $ elems parent ++
                                     [newPath (level-shiftStep) tail])
                      else BodyNode (parent //
                              [(subIx, pushTail cnt (level-shiftStep) (parent ! subIx) tail)])

newPath :: Int -> Array Int e -> Node e
newPath 0 t = LeafNode t
newPath s t = BodyNode $ listArray (0, 0) [newPath (s-shiftStep) t]


update :: Int -> e -> PVector e -> PVector e
update ix el pv | ix >= cnt pv = throw $ IndexOutOfBounds ""
                | ix >= chunk  = throw $ IndexOutOfBounds "TODO"
                | otherwise    = PV (cnt pv) (shift pv) (root pv) (tail pv // [(ix, el)])

tailOff :: Int -> Int
tailOff count = if count < chunk
                   then 0
                   else (count-1) .&. (complement mask)

toList :: PVector e -> [e]
toList pv = [pv `index` x | x <- [0..(cnt pv)-1]]
