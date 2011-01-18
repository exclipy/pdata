-----------------------------------------------------------------------------
-- |
-- Module      :  Data.HamtMap
-- Copyright   :  (c) Kevin Wu Won 2011
-- License     :  BSD-style
-- Maintainer  :  exclipy@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- An implementation of maps from keys to values (dictionaries) based on the
-- hash array mapped trie.
--
-- Since many function names (but not the type name) clash with
-- "Prelude" names, this module is usually imported @qualified@, e.g.
--
-- >  import qualified Data.HamtMap as HM
--
-- This data structure is based on Phil Bagwell's hash array mapped trie,
-- which is described by his original paper:
--
--    * <http://lampwww.epfl.ch/papers/idealhashtrees.pdf>
-----------------------------------------------------------------------------

module Data.HamtMap (
    -- * HamtMap type
      HamtMap
    -- * Operators
    , (Data.HamtMap.!)
    -- * Query
    , member
    , notMember
    , Data.HamtMap.lookup
    -- * Construction
    , empty
    , singleton
    -- * Insertion
    , insert
    , insertWith
    -- * Delete\/Update
    , Data.HamtMap.delete
    , adjust
    , update
    , alter
    -- * Traversal
    , Data.HamtMap.map
    , mapWithKey
    -- * Filter
    , Data.HamtMap.filter
    , filterWithKey
    -- * Conversion
    , Data.HamtMap.elems
    , keys
    , toList
    , fromListWith
    , fromList
    ) where

import Data.BitUtil
import Control.Monad
import Control.DeepSeq
import Data.Bits
import Data.Int
import Data.List hiding (insert, lookup)
import Data.Array as A
import Prelude as P

-- | A HamtMap from keys @k@ to values @v@
data (Eq k) => HamtMap k v = HM {
                                    hashFn :: k -> Int32
                                  , root :: Node k v
                              }

instance (Eq k, Show k, Show v) => Show (HamtMap k v) where
    show (HM h r) = show r
    -- show = ("fromList hashFn "++).show.(Data.HamtMap.toList)

instance (Eq k, NFData k, NFData v) => NFData (HamtMap k v) where
    rnf (HM f r) = f `seq` rnf r

instance (Eq k, NFData k, NFData v) => NFData (Node k v) where
    rnf EmptyNode = ()
    rnf (LeafNode h k v) = rnf h `seq` rnf k `seq` rnf v
    rnf (HashCollisionNode h xs) = rnf h `seq` rnf xs
    rnf (BitmapIndexedNode bm arr) = rnf bm `seq` rnf arr
    rnf (ArrayNode n arr) = rnf n `seq` rnf arr

data (Eq k) => Node k v = EmptyNode |
                          LeafNode {
                                hash :: Int32
                              , key :: k
                              , value :: v
                          } |
                          HashCollisionNode {
                                hash :: Int32
                              , pairs :: [(k, v)]
                          } |
                          BitmapIndexedNode {
                                bitmap :: Int32
                              , subNodes :: Array Int32 (Node k v)
                          } |
                          ArrayNode {
                                numChildren :: Int32
                              , subNodes :: Array Int32 (Node k v)
                          }

instance (Eq k, Show k, Show v) => Show (Node k v) where
    show EmptyNode = ""
    show (LeafNode _hash key value) = show (key, value)
    show (HashCollisionNode _hash pairs) = "h" ++ show pairs
    show (BitmapIndexedNode bitmap subNodes) = "b" ++ show bitmap ++ (show $ A.elems subNodes)
    show (ArrayNode numChildren subNodes) = "a" ++ show numChildren ++ (show $ A.elems subNodes)


-- Some constants
shiftStep = 5
chunk = 2^shiftStep
mask = pred chunk
bmnodeMax = 16 -- maximum size of a BitmapIndexedNode
arraynodeMin = 8  -- minimum size of an ArrayNode

-- Some miscellaneous helper functions

isEmptyNode :: Node k v -> Bool
isEmptyNode EmptyNode = True
isEmptyNode _ = False


isTipNode :: (Eq k) => Node k v -> Bool
isTipNode EmptyNode               = True
isTipNode (LeafNode _ _ _)        = True
isTipNode (HashCollisionNode _ _) = True
isTipNode _                       = False


hashFragment shift hash = (hash `shiftR` shift) .&. fromIntegral mask


-- | @('empty' hashFn)@ is the empty HamtMap, with hashFn being the key hash function.
empty :: (Eq k) => (k -> Int32) -> HamtMap k v

empty hashFn = HM hashFn EmptyNode


-- | @('singleton' hashFn key value)@ is a single-element HamtMap holding @(key, value)@
singleton :: (Eq k) => (k -> Int32) -> k -> v -> HamtMap k v

singleton hashFn key value = HM hashFn $ LeafNode (hashFn key) key value


-- Helper data type for alterNode
data Change = Removed | Modified | Nil | Added deriving Eq


-- | The expression (@'alter' f k map@) alters the value @x@ at @k@, or absence thereof.
-- 'alter' can be used to insert, delete, or update a value in a 'Map'.
-- In short : @'lookup' k ('alter' f k m) = f ('lookup' k m)@.
alter :: (Eq k) => (Maybe v -> Maybe v) -> k -> HamtMap k v -> HamtMap k v

alter updateFn key (HM hashFn root) =
    HM hashFn $ alterNode 0 updateFn (hashFn key) key root


alterNode :: (Eq k) => Int -> (Maybe v -> Maybe v) -> Int32 -> k -> Node k v -> Node k v

alterNode _shift updateFn hash key EmptyNode =
    maybe EmptyNode
          (LeafNode hash key)
          (updateFn Nothing)

alterNode shift updateFn hash' key' node@(LeafNode hash key value) =
    if key' == key
       then maybe EmptyNode
                  (LeafNode hash key)
                  (updateFn (Just value))
       else let node' = alterNode shift updateFn hash' key' EmptyNode
                in if isEmptyNode node'
                      then node
                      else combineNodes shift node node'
    where
    combineNodes :: (Eq k) => Int -> Node k v -> Node k v -> Node k v
    combineNodes shift node1@(LeafNode h1 k1 v1) node2@(LeafNode h2 k2 v2) =
        let hash1 = nodeHash node1
            hash2 = nodeHash node2
            subHash1 = hashFragment shift hash1
            subHash2 = hashFragment shift hash2
            (nodeA, nodeB) = if (subHash1 < subHash2)
                                then (node1, node2)
                                else (node2, node1)
            bitmap' = ((toBitmap subHash1) .|. (toBitmap subHash2))
            subNodes' = if subHash1 == subHash2
                           then listArray (0, 0) [combineNodes (shift+shiftStep) node1 node2]
                           else listArray (0, 1) [nodeA, nodeB]
            in if hash1 == hash2
                  then HashCollisionNode hash1 [(k2, v2), (k1, v1)]
                  else BitmapIndexedNode bitmap' subNodes'
    nodeHash (LeafNode hash key value) = hash
    nodeHash (HashCollisionNode hash pairs) = hash

alterNode _shift updateFn _hash' key (HashCollisionNode hash pairs) =
    let pairs' = updateList updateFn key pairs
        in case pairs' of
                []             -> undefined -- should never happen
                [(key, value)] -> LeafNode hash key value
                otherwise      -> HashCollisionNode hash pairs'
    where updateList updateFn key [] =
              maybe []
                    (\value' -> [(key, value')])
                    (updateFn Nothing)
          updateList updateFn key' ((key, value):pairs) | key' == key =
              maybe pairs
                    (\value' -> (key, value'):pairs)
                    (updateFn (Just value))
          updateList updateFn key (p:pairs) =
              p : updateList updateFn key pairs

alterNode shift updateFn hash key bmnode@(BitmapIndexedNode bitmap subNodes) =
    let subHash = hashFragment shift hash
        ix = fromBitmap bitmap subHash
        bit = toBitmap subHash
        exists = (bitmap .&. bit) /= 0
        child = if exists then subNodes A.! fromIntegral ix else EmptyNode
        child' = alterNode (shift+shiftStep) updateFn hash key child
        removed = exists && isEmptyNode child'
        added = not exists && not (isEmptyNode child')
        change = if exists
                    then if isEmptyNode child'
                            then Removed
                            else Modified
                 else if isEmptyNode child'
                    then Nil
                    else Added
        bound = snd $ bounds subNodes
        bound' = case change of
                      Removed  -> bound-1
                      Modified -> bound
                      Nil      -> bound
                      Added    -> bound+1
        (left, right) = splitAt ix $ A.elems subNodes
        subNodes' = case change of
                         Removed  -> listArray (0, bound') $ left ++ (tail right)
                         Modified -> subNodes // [(fromIntegral ix, child')]
                         Nil      -> subNodes
                         Added    -> listArray (0, bound') $ left ++ (child':right)
        bitmap' = case change of
                       Removed  -> bitmap .&. (complement bit)
                       Modified -> bitmap
                       Nil      -> bitmap
                       Added    -> bitmap .|. bit
        in if bitmap' == 0
              then -- Remove an empty BitmapIndexedNode
                   -- Note: it's possible to have a single-element BitmapIndexedNode
                   -- if there are two keys with the same subHash in the trie.
                   EmptyNode
           else if bound' == 0 && isTipNode (subNodes' A.! 0)
              then -- Pack a BitmapIndexedNode into a LeafNode
                   subNodes' A.! 0
           else if change == Added && bound' > bmnodeMax - 1
              then -- Expand a BitmapIndexedNode into an ArrayNode
                   expandBitmapNode shift subHash child' bitmap subNodes
              else BitmapIndexedNode bitmap' subNodes'
    where
    expandBitmapNode :: (Eq k) =>
        Int -> Int32 -> Node k v -> Int32 -> Array Int32 (Node k v) -> Node k v
    expandBitmapNode shift subHash node' bitmap subNodes =
        let assocs = zip (bitmapToIndices bitmap) (A.elems subNodes)
            assocs' = (subHash, node'):assocs
            blank = listArray (0, 31) $ replicate 32 EmptyNode
            numChildren = (bitCount32 bitmap) + 1
            in ArrayNode numChildren $ blank // assocs'
            -- TODO: an array copy could be avoided here

alterNode shift updateFn hash key node@(ArrayNode numChildren subNodes) =
    let subHash = hashFragment shift hash
        child = subNodes A.! subHash
        child' = alterNode (shift+shiftStep) updateFn hash key child
        change = if isEmptyNode child
                    then if isEmptyNode child'
                            then Nil
                            else Added
                 else if isEmptyNode child'
                    then Removed
                    else Modified
        numChildren' = case change of
                            Removed  -> numChildren-1
                            Modified -> numChildren
                            Nil      -> numChildren
                            Added    -> numChildren+1
        in if numChildren' < arraynodeMin
              -- Pack an ArrayNode into a BitmapIndexedNode when usage drops below 25%
              then packArrayNode subHash numChildren subNodes
              else ArrayNode numChildren' $ subNodes // [(subHash, child')]
    where
    packArrayNode :: (Eq k) => Int32 -> Int32 -> Array Int32 (Node k v) -> Node k v
    packArrayNode subHashToRemove numChildren subNodes =
        let elems' = P.map (\i -> if i == subHashToRemove
                                   then EmptyNode
                                   else subNodes A.! i)
                         [0..pred chunk]
            subNodes' = listArray (0, (numChildren-2)) $ P.filter (not.isEmptyNode) elems'
            listToBitmap = foldr (\on bm -> (bm `shiftL` 1) .|. (if on then 1 else 0)) 0
            bitmap = listToBitmap $ P.map (not.isEmptyNode) elems'
            in BitmapIndexedNode bitmap subNodes'


-- | Insert with a function, combining new value and old value.
-- @'insertWith' f key value mp@
-- will insert the pair (key, value) into @mp@ if key does
-- not exist in the map. If the key does exist, the function will
-- insert the pair @(key, f new_value old_value)@.
insertWith :: (Eq k) => (v -> v -> v) -> k -> v -> HamtMap k v -> HamtMap k v

insertWith accumFn key value hm =
    let fn :: (v -> v -> v) -> v -> Maybe v -> Maybe v
        fn accumFn x' Nothing = Just x'
        fn accumFn x' (Just x) = Just $ accumFn x' x
        in alter (fn accumFn value) key hm


-- | Insert a new key and value in the map.
-- If the key is already present in the map, the associated value is
-- replaced with the supplied value. 'insert' is equivalent to
-- @'insertWith' 'const'@.
insert :: (Eq k) => k -> v -> HamtMap k v -> HamtMap k v

insert = insertWith const


-- | The expression (@'update' f k map@) updates the value @x@
-- at @k@ (if it is in the map). If (@f x@) is 'Nothing', the element is
-- deleted. If it is (@'Just' y@), the key @k@ is bound to the new value @y@.
update :: (Eq k) => (v -> Maybe v) -> k -> HamtMap k v -> HamtMap k v

update updateFn = alter ((=<<) updateFn)


-- | Delete a key and its value from the map. When the key is not
-- a member of the map, the original map is returned.
delete :: (Eq k) => k -> HamtMap k v -> HamtMap k v

delete = alter (const Nothing)


-- | Update a value at a specific key with the result of the provided function.
-- When the key is not a member of the map, the original map is returned.
adjust :: (Eq k) => (v -> v) -> k -> HamtMap k v -> HamtMap k v

adjust updateFn = alter ((=<<) ((Just).updateFn))


-- | Map a function over all values in the map.
mapWithKey :: (Eq k) => (k -> v -> v) -> HamtMap k v -> HamtMap k v

mapWithKey mapFn (HM hashFn root) =
    HM hashFn $ mapWithKeyNode mapFn root


mapWithKeyNode :: (Eq k) => (k -> v -> v) -> Node k v -> Node k v

mapWithKeyNode _mapFn EmptyNode = EmptyNode

mapWithKeyNode mapFn (LeafNode hash key value) = LeafNode hash key $ mapFn key value

mapWithKeyNode mapFn (HashCollisionNode hash pairs) =
    HashCollisionNode hash (P.map (\(key, value) -> (key, mapFn key value)) pairs)

mapWithKeyNode mapFn (BitmapIndexedNode bitmap subNodes) =
    BitmapIndexedNode bitmap $ arrayMap (mapWithKeyNode mapFn) subNodes

mapWithKeyNode mapFn (ArrayNode numChildren subNodes) =
    ArrayNode numChildren $ arrayMap (mapWithKeyNode mapFn) subNodes


arrayMap :: (Ix i) => (a -> a) -> Array i a -> Array i a

arrayMap fn arr = array (bounds arr) $ P.map (\(key, value) -> (key, fn value)) $ A.assocs arr


-- | Map a function over all values in the map.
map :: (Eq k) => (v -> v) -> HamtMap k v -> HamtMap k v

map fn = mapWithKey (const fn)


-- | Filter for all values that satisify a predicate.
filterWithKey :: (Eq k) => (k -> v -> Bool) -> HamtMap k v -> HamtMap k v

filterWithKey fn (HM hashFn root) =
    HM hashFn $ filterWithKeyNode fn root

filterWithKeyNode :: (Eq k) => (k -> v -> Bool) -> Node k v -> Node k v

filterWithKeyNode _fn EmptyNode = EmptyNode

filterWithKeyNode fn node@(LeafNode hash key value) | fn key value = node
                                                    | otherwise    = EmptyNode

filterWithKeyNode fn (HashCollisionNode hash pairs) =
    let pairs' = P.filter (uncurry fn) pairs
        in case pairs' of
                []             -> EmptyNode
                [(key, value)] -> LeafNode hash key value
                otherwise      -> HashCollisionNode hash pairs'

filterWithKeyNode fn (BitmapIndexedNode bitmap subNodes) =
    let mapped = P.map (filterWithKeyNode fn) (A.elems subNodes)
        zipped = zip (bitmapToIndices bitmap) mapped
        filtered = P.filter (\(ix, subNode) -> not (isEmptyNode subNode)) zipped
        (indices', subNodes') = unzip filtered
        n = fromIntegral $ length filtered
        in case subNodes' of
                []                      -> EmptyNode
                [node] | isTipNode node -> node
                otherwise               -> BitmapIndexedNode (indicesToBitmap indices')
                                                             (listArray (0, n-1) subNodes')

filterWithKeyNode fn (ArrayNode numChildren subNodes) =
    let mapped = P.map (filterWithKeyNode fn) (A.elems subNodes)
        zipped = zip [0..31] mapped
        filtered = P.filter (\(ix, subNode) -> not (isEmptyNode subNode)) zipped
        (indices', subNodes') = unzip filtered
        n = fromIntegral $ length filtered
        in case filtered of
                [] -> EmptyNode
                [(ix, node)] | isTipNode node -> node
                els | n <= bmnodeMax -> BitmapIndexedNode (indicesToBitmap indices')
                                                          (listArray (0, n-1) subNodes')
                    | otherwise      -> ArrayNode n (listArray (0, 31) mapped)


-- | Filter for all values that satisify a predicate.
filter :: (Eq k) => (v -> Bool) -> HamtMap k v -> HamtMap k v

filter fn = filterWithKey (const fn)

-- | Lookup the value at a key in the map.
--
-- The function will return the corresponding value as @('Just' value)@,
-- or 'Nothing' if the key isn't in the map.
lookup :: (Eq k) => k -> HamtMap k v -> Maybe v

lookup key (HM hashFn root) = lookupNode 0 (hashFn key) key root


lookupNode :: (Eq k) => Int -> Int32 -> k -> Node k v -> Maybe v

lookupNode _ _ _ EmptyNode = Nothing

lookupNode _ _ key' (LeafNode _ key value) =
    if key' == key then Just value
                        else Nothing

lookupNode _ _ key (HashCollisionNode _ pairs) =
    P.lookup key pairs

lookupNode shift hash key (BitmapIndexedNode bitmap subNodes) =
    let subHash = hashFragment shift hash
        ix = fromBitmap bitmap subHash
        exists = (bitmap .&. (toBitmap subHash)) /= 0
        in if exists
              then lookupNode (shift+shiftStep) hash key (subNodes A.! ix)
              else Nothing

lookupNode shift hash key (ArrayNode _numChildren subNodes) =
    let subHash = hashFragment shift hash
        in lookupNode (shift+shiftStep) hash key (subNodes A.! subHash)


-- | Find the value at a key.
-- Calls 'error' when the element can not be found.
(!) :: (Eq k) => HamtMap k v -> k -> v

hm ! key = maybe (error "element not in the map")
                 id
                 (Data.HamtMap.lookup key hm)


-- | Is the key a member of the map? See also 'notMember'.
member :: (Eq k) => k -> HamtMap k v -> Bool

member key hm = maybe False (const True) (Data.HamtMap.lookup key hm)

-- | Is the key a member of the map? See also 'member'.
notMember :: (Eq k) => k -> HamtMap k v -> Bool

notMember key = not.(member key)


-- | Convert to a list of key\/value pairs.
toList :: (Eq k) => HamtMap k v -> [(k, v)]

toList (HM _hashFn root) = toListNode root


toListNode :: (Eq k) => Node k v -> [(k, v)]

toListNode EmptyNode = []

toListNode (LeafNode _hash key value) = [(key, value)]

toListNode (HashCollisionNode _hash pairs) = pairs

toListNode (BitmapIndexedNode _bitmap subNodes) =
    concat $ P.map toListNode $ A.elems subNodes

toListNode (ArrayNode _numChildren subNodes) =
    concat $ P.map toListNode $ A.elems subNodes


-- | Build a map from a list of key\/value pairs with a combining function.
fromListWith :: (Eq k) => (k -> Int32) -> (v -> v -> v) -> [(k, v)] -> HamtMap k v

fromListWith hashFn combineFn assocs =
    HM hashFn $ fromListNode 0 combineFn $ P.map (\(k, v) -> ((hashFn k), k, v)) assocs


fromListNode :: (Eq k) => Int -> (v -> v -> v) -> [(Int32, k, v)] -> Node k v

fromListNode shift combineFn hkvs =
    let subHashed = P.map (\triple@(h, k, v) -> (hashFragment shift h, triple)) hkvs
        divided = accumArray (flip (:)) [] (0, mask) subHashed
                  -- this will alternately reverse and unreverse the list on each level down
        dividedList = A.elems divided
        subNodes = listArray (0, mask) $ P.map (fromListNode (shift+shiftStep) combineFn) $ dividedList
        numChildren = length $ P.filter (not.null) dividedList
        in case hkvs of
                []          -> EmptyNode
                [(h, k, v)] -> LeafNode h k v
                (h, k, v):hkvs' | all (\(h', _, _) -> h' == h) hkvs' ->
                    if all (\(_, k', _) -> k' == k) hkvs'
                       then let combineFn' = if even shift then flip combineFn else combineFn
                                           -- correct for the alternate reversing of the list
                                v' = foldl1' combineFn' (P.map (\(_, _, v) -> v) hkvs)
                                in LeafNode h k v'
                       else let keyCmp (k1, _) (k2, _) = k1 == k2
                                collisions = P.map (\(_, k', v') -> (k', v')) hkvs
                                grouped = groupBy' keyCmp collisions
                                combineFn' = if even shift then flip combineFn else combineFn
                                collisionKeys = P.map (fst.head) grouped
                                collisionVals = P.map ((foldl1' combineFn').(P.map snd)) grouped
                                collisions' = zip collisionKeys collisionVals
                                in HashCollisionNode h collisions'
                _ | numChildren > fromIntegral bmnodeMax  ->
                    ArrayNode (fromIntegral numChildren) subNodes
                _ | otherwise ->
                    makeBMNode numChildren subNodes
    where
    makeBMNode :: (Eq k) => Int -> Array Int32 (Node k v) -> Node k v
    makeBMNode numChildren subNodes =
        let subNodeList = A.elems subNodes
            subNodes' = listArray (0, (fromIntegral numChildren-1)) $ P.filter (not.isEmptyNode) subNodeList
            listToBitmap = foldr (\on bm -> (bm `shiftL` 1) .|. (if on then 1 else 0)) 0
            bitmap = listToBitmap $ P.map (not.isEmptyNode) subNodeList
            in BitmapIndexedNode bitmap subNodes'

    -- groupBy' is like Data.List.groupBy, but also groups non-adjacent elements
    groupBy' :: (a -> a -> Bool) -> [a] -> [[a]]
    groupBy' eq list = P.map reverse $ foldl' (insertGrouped eq) [] list

    insertGrouped :: (a -> a -> Bool) -> [[a]] -> a -> [[a]]
    insertGrouped eq [] y = [[y]]
    insertGrouped eq ((x:xs):gs) y | eq x y    = (y:x:xs) : gs
                                   | otherwise = (x:xs) : insertGrouped eq gs y


-- | Build a map from a list of key\/value pairs.
-- If the list contains more than one value for the same key, the last value
-- for the key is retained.
fromList :: (Eq k) => (k -> Int32) -> [(k, v)] -> HamtMap k v

fromList hashFn assocs =
    fromListWith hashFn const assocs



-- | Return all keys of the map.
keys :: (Eq k) => HamtMap k v -> [k]

keys = (P.map fst).toList


-- | Return all elements of the map.
elems :: (Eq k) => HamtMap k v -> [v]

elems = (P.map snd).toList