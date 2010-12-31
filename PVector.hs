module PVector (
    PVector,
    empty,
    (PVector.!),
    append,
    adjust,
    set,
    PVector.map,
    PVector.elems) where

import Data.Array as A
import Data.Bits hiding (shift)
import Control.Exception
import Prelude as P hiding (tail)
import Data.List

-- Some constants
shiftStep = 5
chunk = 2^shiftStep
mask = pred chunk

data PVector e = PV {
      cnt :: Int
    , shift :: Int
    , root :: Node e
    , tail :: Array Int e
    }

data Node e = BodyNode (Array Int (Node e)) |
              LeafNode (Array Int e)

instance (Show e) => Show (PVector e) where
    show = ("fromList "++).show.(PVector.elems)

-- (empty) is a PVector with nothing in it
empty :: PVector e
empty = PV 0 shiftStep (BodyNode (array (0, -1) [])) (array (0, -1) [])

-- (pv ! ix) is the element of pv at index ix
(!) :: PVector e -> Int -> e
(PV c s r t) ! ix | ix >= c || ix < 0 = throw $ IndexOutOfBounds ""
                  | ix >= tailOff c   = t A.! (ix - tailOff c)
                  | otherwise         = lookup r s ix
    where lookup :: Node e -> Int -> Int -> e
          lookup node level ix = let subIx = (ix `shiftR` level) .&. mask
                                     in case node of
                                             BodyNode a -> lookup (a A.! subIx) (level-shiftStep) ix
                                             LeafNode a -> a A.! subIx

-- (append el pv) is pv, with el appended
append :: e -> PVector e -> PVector e
append el (PV c s r t) =
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
                                       pushTail cnt (level-shiftStep) (parent A.! subIx) tail)])
                  in BodyNode array

          newPath :: Int -> Array Int e -> Node e
          newPath 0 t = LeafNode t
          newPath s t = BodyNode $ listArray (0, 0) [newPath (s-shiftStep) t]

-- (adjust fn ix pv) is pv with the element at index ix modified using fn
adjust ::  (e -> e) ->Int -> PVector e -> PVector e
adjust fn ix (PV c s r t) | ix >= c || ix < 0 = throw $ IndexOutOfBounds ""
                          | ix >= tailOff c   = let tailIx = ix - tailOff c
                                                    el = t A.! tailIx
                                                    in PV c s r (t // [(tailIx, fn el)])
                          | otherwise         = PV c s (modify r s ix fn) t
    where modify :: Node e -> Int -> Int -> (e -> e) -> Node e
          modify (BodyNode a) level ix fn =
              let subIx = (ix `shiftR` level) .&. mask
                  in BodyNode (a // [(subIx, modify (a A.! subIx) (level-shiftStep) ix fn)])
          modify (LeafNode a) level ix fn =
              let subIx = (ix `shiftR` level) .&. mask
                  el = a A.! subIx
                  in LeafNode $ a // [(subIx, fn el)]

-- (set ix el pv) is pv with el at index ix
set :: Int -> e -> PVector e -> PVector e
set ix e = adjust (const e) ix


-- (map fn pv) is pv with every element el replaced with (fn el)
map :: (e -> e) -> PVector e -> PVector e
map fn (PV c s r t) = PV c s (mapNode fn r) (arrayMap fn t)
    where mapNode fn (BodyNode a) = BodyNode $ arrayMap (mapNode fn) a
          mapNode fn (LeafNode a) = LeafNode $ arrayMap fn a


arrayMap :: (Ix i) => (a -> a) -> Array i a -> Array i a
arrayMap fn arr = array (bounds arr) $ P.map (\(key, value) -> (key, fn value)) $ A.assocs arr

-- (elems pv) is a list of the elements of pv
elems :: PVector e -> [e]
elems (PV c s r t) = elemsNode r ++ A.elems t
    where elemsNode (BodyNode arr) = concat $ P.map elemsNode $ A.elems arr
          elemsNode (LeafNode arr) = A.elems arr


-- (fromList list) is a PVector equivalent to list
fromList :: [e] -> PVector e
fromList = foldl' (flip append) empty
-- TODO: make this more efficient by using a transient array


-- Private Stuff

-- Internal functions

tailOff :: Int -> Int
tailOff count = if count < chunk
                   then 0
                   else (count-1) .&. (complement mask)

arrayAppend :: (Ix i, Enum i) => Array i e -> e -> Array i e
arrayAppend a el = let (lower, upper) = bounds a
                       in listArray (lower, succ upper) (A.elems a ++ [el])
