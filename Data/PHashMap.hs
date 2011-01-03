module Data.PHashMap (
    -- * PHashMap type
      PHashMap
    -- * Operators
    , (Data.PHashMap.!)
    -- * Query
    , member
    , notMember
    , Data.PHashMap.lookup
    -- * Construction
    , empty
    , singleton
    -- * Insertion
    , insert
    , insertWith
    -- * Delete\/Update
    , Data.PHashMap.delete
    , adjust
    , update
    , alter
    -- * Traversal
    , Data.PHashMap.map
    , mapWithKey
    -- * Conversion
    , Data.PHashMap.elems
    , keys
    , toList
    , fromList
    ) where

import BitUtil
import Data.Bits
import Data.Int
import Data.List hiding (insert, lookup)
import Data.Array as A
import Prelude as P
import Control.Monad

-- Some constants
shiftStep = 5
chunk = 2^shiftStep
mask = pred chunk


-- | A PHashMap from keys @k@ to values @v@
data (Eq k) => PHashMap k v = PHM {
                                    hashFn :: k -> Int32
                                  , root :: Node k v
                              }

instance (Eq k, Show k, Show v) => Show (PHashMap k v) where
    show = ("fromList hashFn "++).show.(Data.PHashMap.toList)

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


-- Some miscellaneous helper functions

isEmptyNode :: Node k v -> Bool
isEmptyNode EmptyNode = True
isEmptyNode _ = False

hashFragment shift hash = (hash `shiftR` shift) .&. fromIntegral mask


-- | @('empty' hashFn)@ is the empty PHashMap, with hashFn being the key hash function.
empty :: (Eq k) => (k -> Int32) -> PHashMap k v

empty hashFn = PHM hashFn EmptyNode


-- | @('singleton' hashFn key value)@ is a single-element PHashMap holding @(key, value)@
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



-- | The expression (@'alter' f k map@) alters the value @x@ at @k@, or absence thereof.
-- 'alter' can be used to insert, delete, or update a value in a 'Map'.
-- In short : @'lookup' k ('alter' f k m) = f ('lookup' k m)@.
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


-- | Insert with a function, combining new value and old value.
-- @'insertWith' f key value mp@
-- will insert the pair (key, value) into @mp@ if key does
-- not exist in the map. If the key does exist, the function will
-- insert the pair @(key, f new_value old_value)@.
insertWith :: (Eq k) => (v -> v -> v) -> k -> v -> PHashMap k v -> PHashMap k v

insertWith accumFn key value hashMap =
    let fn :: (v -> v -> v) -> v -> Maybe v -> Maybe v
        fn accumFn x' Nothing = Just x'
        fn accumFn x' (Just x) = Just $ accumFn x' x
        in alter (fn accumFn value) key hashMap


-- | Insert a new key and value in the map.
-- If the key is already present in the map, the associated value is
-- replaced with the supplied value. 'insert' is equivalent to
-- @'insertWith' 'const'@.
insert :: (Eq k) => k -> v -> PHashMap k v -> PHashMap k v

insert = insertWith const


-- | The expression (@'update' f k map@) updates the value @x@
-- at @k@ (if it is in the map). If (@f x@) is 'Nothing', the element is
-- deleted. If it is (@'Just' y@), the key @k@ is bound to the new value @y@.
update :: (Eq k) => (v -> Maybe v) -> k -> PHashMap k v -> PHashMap k v

update updateFn = alter ((=<<) updateFn)


-- | Delete a key and its value from the map. When the key is not
-- a member of the map, the original map is returned.
delete :: (Eq k) => k -> PHashMap k v -> PHashMap k v

delete = alter (const Nothing)


-- | Update a value at a specific key with the result of the provided function.
-- When the key is not a member of the map, the original map is returned.
adjust :: (Eq k) => (v -> v) -> k -> PHashMap k v -> PHashMap k v

adjust updateFn = update ((Just).updateFn)


-- | Map a function over all values in the map.
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


-- | Map a function over all values in the map.
map :: (Eq k) => (v -> v) -> PHashMap k v -> PHashMap k v

map fn = mapWithKey (const fn)


arrayMap :: (Ix i) => (a -> a) -> Array i a -> Array i a

arrayMap fn arr = array (bounds arr) $ P.map (\(key, value) -> (key, fn value)) $ A.assocs arr


-- | Lookup the value at a key in the map.
--
-- The function will return the corresponding value as @('Just' value)@,
-- or 'Nothing' if the key isn't in the map.
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


-- | Find the value at a key.
-- Calls 'error' when the element can not be found.
(!) :: (Eq k) => PHashMap k v -> k -> v

hashMap ! key = maybe (error "element not in the map")
                      id
                      (Data.PHashMap.lookup key hashMap)


-- | Is the key a member of the map? See also 'notMember'.
member :: (Eq k) => k -> PHashMap k v -> Bool

member key hashMap = maybe False (const True) (Data.PHashMap.lookup key hashMap)

-- | Is the key a member of the map? See also 'member'.
notMember :: (Eq k) => k -> PHashMap k v -> Bool

notMember key = not.(member key)


-- | Convert to a list of key\/value pairs.
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


-- | Build a map from a list of key\/value pairs.
-- If the list contains more than one value for the same key, the last value
-- for the key is retained.
fromList :: (Eq k) => (k -> Int32) -> [(k, v)] -> PHashMap k v

fromList hashFn = foldl' (\hm (key, value) -> insert key value hm)
                         (empty hashFn)
                  -- TODO: make this more efficient by using a transient array


-- | Return all keys of the map.
keys :: (Eq k) => PHashMap k v -> [k]

keys = (P.map fst).toList


-- | Return all elements of the map.
elems :: (Eq k) => PHashMap k v -> [v]

elems = (P.map snd).toList
