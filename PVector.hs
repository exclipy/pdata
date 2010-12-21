module PVector (PVector, empty, (|>), index, update, toList) where

import Data.Array hiding (index)
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
    show = show.toList

-- (empty) is a PVector with nothing in it
empty :: PVector e
empty = PV 0 shiftStep (BodyNode (array (0, -1) [])) (array (0, -1) [])

-- (pv `index` ix) is the element at index ix
index :: PVector e -> Int -> e
(PV c s r t) `index` ix | ix >= c || ix < 0 = throw $ IndexOutOfBounds ""
                        | ix >= tailOff c   = t ! (ix - tailOff c)
                        | otherwise         = lookup r s ix
    where lookup :: Node e -> Int -> Int -> e
          lookup node level ix = let subIx = (ix `shiftR` level) .&. mask
                                     in case node of
                                             BodyNode a -> lookup (a ! subIx) (level-shiftStep) ix
                                             LeafNode a -> a ! subIx

-- (pv |> el) is a new PVector the same as pv, except with el appended
(|>) :: PVector e -> e -> PVector e
(PV c s r t) |> el =
    let tailIx = c - tailOff c
        in if tailIx < chunk
              then let newTail = arrayAppend t el
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

    where pushTail :: Int -> Int -> Node e -> Array Int e -> Node e
          pushTail cnt level (BodyNode parent) tail =
              let subIx = ((cnt-1) `shiftR` level) .&. mask
                  array = if level == shiftStep
                             then arrayAppend parent $ LeafNode tail
                             else if subIx > snd (bounds parent)
                                  then arrayAppend parent $ newPath (level-shiftStep) tail
                                       else (parent // [(subIx,
                                            pushTail cnt (level-shiftStep) (parent ! subIx) tail)])
                  in BodyNode array

          newPath :: Int -> Array Int e -> Node e
          newPath 0 t = LeafNode t
          newPath s t = BodyNode $ listArray (0, 0) [newPath (s-shiftStep) t]

-- (update ix el pv) is a PVector the same as pv, except with el at index ix
update :: Int -> e -> PVector e -> PVector e
update ix el (PV c s r t) | ix >= c || ix < 0 = throw $ IndexOutOfBounds ""
                          | ix >= tailOff c   = PV c s r (t // [(ix - tailOff c, el)])
                          | otherwise         = PV c s (modify r s ix el) t
    where modify :: Node e -> Int -> Int -> e -> Node e
          modify node level ix el = let subIx = (ix `shiftR` level) .&. mask
                                    in case node of
                                            BodyNode a -> BodyNode (a // [(subIx,
                                                modify (a ! subIx) (level-shiftStep) ix el)])
                                            LeafNode a -> LeafNode $ a // [(subIx, el)]

-- (toList pv) is a list of the elements of pv
toList :: PVector e -> [e]
toList pv = [pv `index` x | x <- [0..(cnt pv)-1]]


-- Private Stuff

-- Some constants
shiftStep = 5
chunk = 2^shiftStep
mask = pred chunk

-- What a PVector is made of
data Node e = BodyNode (Array Int (Node e)) |
              LeafNode (Array Int e)

-- Internal functions

tailOff :: Int -> Int
tailOff count = if count < chunk
                   then 0
                   else (count-1) .&. (complement mask)

arrayAppend :: (Ix i, Enum i) => Array i e -> e -> Array i e
arrayAppend a el = let (lower, upper) = bounds a
                       in listArray (lower, succ upper) (elems a ++ [el])
