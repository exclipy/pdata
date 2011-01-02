module Data.PHashMap (
    PHashMap,
    empty,
    singleton,
    alter,
    insert,
    insertWith,
    update,
    Data.PHashMap.delete,
    adjust,
    Data.PHashMap.lookup,
    (Data.PHashMap.!),
    mapWithKey,
    Data.PHashMap.map,
    member,
    notMember,
    keys,
    Data.PHashMap.elems,
    toList,
    fromList) where

import BitUtil
import Data.Bits
import Data.Int
import Data.List hiding (insert, lookup)
import Data.Array as A
import Prelude as P
import Control.Exception
import Control.Monad

-- Some constants
shiftStep = 5
chunk = 2^shiftStep
mask = pred chunk


data (Eq k) => PHashMap k v = PHM {
                                  hashFn :: k -> Int32,
                                  root :: Node k v
                              }

instance (Eq k, Show k, Show v) => Show (PHashMap k v) where
    show = ("fromList hashFn "++).show.(Data.PHashMap.toList)

data (Eq k) => Node k v = EmptyNode |
                          LeafNode {
                              hash :: Int32,
                              key :: k,
                              value :: v
                          } |
                          HashCollisionNode {
                              hash :: Int32,
                              pairs :: [(k, v)]
                          } |
                          BitmapIndexedNode {
                              bitmap :: Int32,
                              subNodes :: Array Int32 (Node k v)
                          } |
                          ArrayNode {
                              numChildren :: Int32,
                              subNodes :: Array Int32 (Node k v)
                          }

instance (Eq k, Show k, Show v) => Show (Node k v) where
    show EmptyNode = ""
    show (LeafNode _hash key value) = show (key, value)
    show (HashCollisionNode _hash pairs) = "h" ++ show pairs
    show (BitmapIndexedNode bitmap subNodes) = "b" ++ show bitmap ++ (show $ A.elems subNodes)
    show (ArrayNode numChildren subNodes) = "a" ++ show numChildren ++ (show $ A.elems subNodes)


-- Some miscellaneous helper functions

isEmptyNode :: Node k v -> Bool
isEmptyNode EmptyNode = True
isEmptyNode _ = False

hashFragment shift hash = (hash `shiftR` shift) .&. fromIntegral mask


-- (empty hashFn) is the empty PHashMap, with hashFn being the key hash function
empty :: (Eq k) => (k -> Int32) -> PHashMap k v

empty hashFn = PHM hashFn EmptyNode


-- (singleton hashFn key value) is the single-element PHashMap containing only (key, value)
singleton :: (Eq k) => (k -> Int32) -> k -> v -> PHashMap k v

singleton hashFn key value = PHM hashFn $ LeafNode (hashFn key) key value


-- Helper data type for alterNode
data Change = Removed | Modified | Nil | Added deriving Eq


-- A helper function for alterNode
combineNodes :: (Eq k) => Int -> Node k v -> Node k v -> Node k v

combineNodes shift node node' =
    let subHash = hashFragment shift (nodeHash node)
        subHash2 = hashFragment shift (nodeHash node')
        (nodeA, nodeB) = if (subHash < subHash2)
                            then (node, node')
                            else (node', node)
        bitmap' = ((toBitmap subHash) .|. (toBitmap subHash2))
        subNodes' = if subHash == subHash2
                       then listArray (0, 0) [combineNodes (shift+shiftStep) node node']
                       else listArray (0, 1) [nodeA, nodeB]
        in BitmapIndexedNode bitmap' subNodes'

    where
    nodeHash (LeafNode hash key value) = hash
    nodeHash (HashCollisionNode hash pairs) = hash


-- (alter updateFn key hashMap) is hashMap with the value at key updated using updateFn.
-- If updateFn returns Nothing, then the key-value pair is removed
alter :: (Eq k) => (Maybe v -> Maybe v) -> k -> PHashMap k v -> PHashMap k v

alter updateFn key (PHM hashFn root) =
    PHM hashFn $ alterNode 0 updateFn (hashFn key) key root


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

alterNode _shift updateFn _hash' key (HashCollisionNode hash pairs) =
    let pairs' = updateList updateFn key pairs
        in case pairs' of
                [(key, value)] -> LeafNode hash key value
                otherwise      -> HashCollisionNode hash pairs'
    where updateList updateFn key [] = []
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
           else if bound' == 0 && isLeafNode (subNodes' A.! 0)
              then -- Pack a BitmapIndexedNode into a LeafNode
                   subNodes' A.! 0
           else if change == Added && bound' > 15
              then -- Expand a BitmapIndexedNode into an ArrayNode
                   expandBitmapNode shift subHash child' bitmap subNodes
              else BitmapIndexedNode bitmap' subNodes'
    where
    isLeafNode (LeafNode _ _ _) = True
    isLeafNode _ = False

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
        removed = isEmptyNode child' && not (isEmptyNode child)
        numChildren' = if removed
                          then numChildren - 1
                          else numChildren
        in if numChildren' < fromIntegral chunk `div` 4
              -- Pack an ArrayNode into a HashCollisionNode when usage drops below 25%
              then packArrayNode subHash numChildren subNodes
              else ArrayNode numChildren' $ subNodes // [(subHash, child')]
    where
    packArrayNode :: (Eq k) => Int32 -> Int32 -> Array Int32 (Node k v) -> Node k v
    packArrayNode subHashToRemove numChildren subNodes =
        let elems' = P.map (\i -> if i == subHashToRemove
                                   then EmptyNode
                                   else subNodes A.! i)
                         [0..pred chunk]
            subNodes' = listArray (0, (numChildren-2)) $ filter (not.isEmptyNode) elems'
            listToBitmap = foldr (\on bm -> (bm `shiftL` 1) .|. (if on then 1 else 0)) 0
            bitmap = listToBitmap $ P.map (not.isEmptyNode) elems'
            in BitmapIndexedNode bitmap subNodes'


-- (insertWith accumFn key value hashMap) is hashMap with (key, value) inserted using accumulation
-- function accumFn.  If value v1 is inserted with the same key as an existing value v2, the new
-- value will be v1 `accumFn` v2
insertWith :: (Eq k) => (v -> v -> v) -> k -> v -> PHashMap k v -> PHashMap k v

insertWith accumFn key value hashMap =
    let fn :: (v -> v -> v) -> v -> Maybe v -> Maybe v
        fn accumFn x' Nothing = Just x'
        fn accumFn x' (Just x) = Just $ accumFn x' x
        in alter (fn accumFn value) key hashMap


-- (insert key value hashMap) is hashMap with (key, value) inserted, replacing any previous
-- value with the given key.
insert :: (Eq k) => k -> v -> PHashMap k v -> PHashMap k v

insert = insertWith const


-- (update updateFn key hashMap) is hashMap with the value at key updated using updateFn.
-- If updateFn returns Nothing, then the key-value pair is removed
update :: (Eq k) => (v -> Maybe v) -> k -> PHashMap k v -> PHashMap k v

update updateFn = alter ((=<<) updateFn)


-- (delete updateFn key hashMap) is hashMap with the value at key removed
delete :: (Eq k) => k -> PHashMap k v -> PHashMap k v

delete = alter (const Nothing)


-- (adjust updateFn key hashMap) is hashMap with the value at key updated using updateFn.
adjust :: (Eq k) => (v -> v) -> k -> PHashMap k v -> PHashMap k v

adjust updateFn = update ((Just).updateFn)


-- (mapWithKey fn hashMap) is a hashMap with all values modified using fn
mapWithKey :: (Eq k) => (k -> v -> v) -> PHashMap k v -> PHashMap k v

mapWithKey mapFn (PHM hashFn root) =
    PHM hashFn $ mapWithKeyNode mapFn root


mapWithKeyNode :: (Eq k) => (k -> v -> v) -> Node k v -> Node k v

mapWithKeyNode _mapFn EmptyNode = EmptyNode

mapWithKeyNode mapFn (LeafNode hash key value) = LeafNode hash key $ mapFn key value

mapWithKeyNode mapFn (HashCollisionNode hash pairs) =
    HashCollisionNode hash (P.map (\(key, value) -> (key, mapFn key value)) pairs)

mapWithKeyNode mapFn (BitmapIndexedNode bitmap subNodes) =
    BitmapIndexedNode bitmap $ arrayMap (mapWithKeyNode mapFn) subNodes

mapWithKeyNode mapFn (ArrayNode numChildren subNodes) =
    ArrayNode numChildren $ arrayMap (mapWithKeyNode mapFn) subNodes


-- (map fn hashMap) is a hashMap with all values modified using fn
map :: (Eq k) => (v -> v) -> PHashMap k v -> PHashMap k v

map fn = mapWithKey (const fn)


arrayMap :: (Ix i) => (a -> a) -> Array i a -> Array i a

arrayMap fn arr = array (bounds arr) $ P.map (\(key, value) -> (key, fn value)) $ A.assocs arr


-- (lookup key hashMap) is Just the value stored at the key, or Nothing if no such key exists
lookup :: (Eq k) => k -> PHashMap k v -> Maybe v

lookup key (PHM hashFn root) = lookupNode 0 (hashFn key) key root


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


(!) :: (Eq k) => PHashMap k v -> k -> v

hashMap ! key = maybe (throw (IndexOutOfBounds "element not in the map"))
                      id
                      (Data.PHashMap.lookup key hashMap)


-- (member x hashMap) is True iff x is a key of hashMap
member :: (Eq k) => k -> PHashMap k v -> Bool

member key hashMap = maybe False (const True) (Data.PHashMap.lookup key hashMap)

notMember key = not.(member key)


-- (toList hashMap) is all the key-value pairs in hashMap as a list
toList :: (Eq k) => PHashMap k v -> [(k, v)]

toList (PHM _hashFn root) = toListNode root


toListNode :: (Eq k) => Node k v -> [(k, v)]

toListNode EmptyNode = []

toListNode (LeafNode _hash key value) = [(key, value)]

toListNode (HashCollisionNode _hash pairs) = pairs

toListNode (BitmapIndexedNode _bitmap subNodes) =
    concat $ P.map toListNode $ A.elems subNodes

toListNode (ArrayNode _numChildren subNodes) =
    concat $ P.map toListNode $ A.elems subNodes


-- (fromList hashFn list) is a PHashMap equivalent to list interpreted as a dictionary
fromList :: (Eq k) => (k -> Int32) -> [(k, v)] -> PHashMap k v

fromList hashFn = foldl' (\hm (key, value) -> insert key value hm)
                         (empty hashFn)
                  -- TODO: make this more efficient by using a transient array


-- (keys hashMap) is a list of all keys in hashMap
keys :: (Eq k) => PHashMap k v -> [k]

keys = (P.map fst).toList


-- (elems hashMap) is a list of all values in hashMap
elems :: (Eq k) => PHashMap k v -> [v]

elems = (P.map snd).toList
