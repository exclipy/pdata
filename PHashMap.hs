module PHashMap where
import Data.Bits
import Data.Word
import Data.List
import Data.Array

data (Eq k) => PHashMap k v = PHM {
    hashFn :: k -> Word32,
    root :: Node k v
}

instance (Eq k, Show k, Show v) => Show (PHashMap k v) where
    show (PHM _hashFn root) = show root


empty :: (Eq k) => (k -> Word32) -> PHashMap k v
empty hashFn = PHM hashFn EmptyNode


update :: (Eq k) => k -> v -> PHashMap k v -> PHashMap k v
update key value (PHM hashFn root) = PHM hashFn $ updateNode 0 (hashFn key) key value root


updateNode :: (Eq k) => Int -> Word32 -> k -> v -> Node k v -> Node k v

updateNode _shift hash key value EmptyNode = LeafNode hash key value
updateNode shift hash key value (LeafNode storedHash storedKey storedValue)
    | hash == storedHash = if key == storedKey
                              then (LeafNode hash key value) -- key exists; just update
                              else HashCollisionNode hash [(key, value), (storedKey, storedValue)]
    | otherwise = makeArrayNode shift (hash, key, value) (storedHash, storedKey, storedValue)

updateNode shift _hash key value (HashCollisionNode hash pairs) =
    HashCollisionNode hash ((key,value):pairs)

updateNode shift hash key value (ArrayNode subNodes) =
    let subHash = hashFragment shift hash
        newChild = updateNode (shift+shiftStep) hash key value (subNodes!subHash)
        in ArrayNode $ subNodes // [(subHash, newChild)]


index :: (Eq k) => PHashMap k v -> k -> v

index (PHM hashFn root) key = indexNode 0 (hashFn key) key root


indexNode :: (Eq k) => Int -> Word32 -> k -> Node k v -> v

indexNode _ _ _ EmptyNode = undefined

indexNode _ _ searchKey (LeafNode _ key value) = if searchKey == key then value
                                                                     else undefined
indexNode shift hash searchKey (ArrayNode subNodes) =
    let subHash = hashFragment shift hash
        in indexNode (shift+shiftStep) hash searchKey (subNodes!subHash)

indexNode _ _ searchKey (HashCollisionNode _ pairs) =
    find searchKey pairs
    where find searchKey [] = undefined
          find searchKey ((key, value):pairs) | searchKey == key = value
                                              | otherwise        = find searchKey pairs


makeArrayNode :: (Eq k) => Int -> (Word32, k, v) -> (Word32, k, v) -> Node k v

makeArrayNode shift (hash1, key1, value1) (hash2, key2, value2) =
    let subHash1 = hashFragment shift hash1
        subHash2 = hashFragment shift hash2
        in ArrayNode (listArray (0, mask) [case i of {
            x | x == subHash1 -> LeafNode hash1 key1 value1;
            x | x == subHash2 -> LeafNode hash2 key2 value2;
            otherwise -> EmptyNode } |
                i <- [0..fromIntegral mask]])

hashFragment shift hash = (hash `shiftR` shift) .&. fromIntegral mask

data (Eq k) => Node k v = EmptyNode |
                          LeafNode {
                              hash :: Word32,
                              key :: k,
                              value :: v
                          } |
                          ArrayNode {
                              subNodes :: Array Word32 (Node k v)
                          } |
                          HashCollisionNode {
                              hash :: Word32,
                              pairs :: [(k, v)]
                          }

instance (Eq k, Show k, Show v) => Show (Node k v) where
    show EmptyNode = ""
    show (LeafNode _hash key value) = "(" ++ (show key) ++ ", " ++ (show value) ++ ")"
    show (ArrayNode subNodes) = show $ elems subNodes
    show (HashCollisionNode _hash pairs) = show pairs

-- Some constants
shiftStep = 5
chunk = 2^shiftStep
mask = pred chunk

-- Bit operations

bitPos :: Word32 -> Int
bitPos x = bitCount32 $ pred x

bitCount32 :: Word32 -> Int
bitCount32 x = bitCount8 (fromIntegral (x `shiftR` 24)) +
               bitCount8 (fromIntegral (x `shiftR` 16)) +
               bitCount8 (fromIntegral (x `shiftR` 8)) +
               bitCount8 (fromIntegral x)

bitCount8 :: Word8 -> Int
bitCount8 0 = 0
bitCount8 1 = 1
bitCount8 2 = 1
bitCount8 3 = 2
bitCount8 4 = 1
bitCount8 5 = 2
bitCount8 6 = 2
bitCount8 7 = 3
bitCount8 8 = 1
bitCount8 9 = 2
bitCount8 10 = 2
bitCount8 11 = 3
bitCount8 12 = 2
bitCount8 13 = 3
bitCount8 14 = 3
bitCount8 15 = 4
bitCount8 16 = 1
bitCount8 17 = 2
bitCount8 18 = 2
bitCount8 19 = 3
bitCount8 20 = 2
bitCount8 21 = 3
bitCount8 22 = 3
bitCount8 23 = 4
bitCount8 24 = 2
bitCount8 25 = 3
bitCount8 26 = 3
bitCount8 27 = 4
bitCount8 28 = 3
bitCount8 29 = 4
bitCount8 30 = 4
bitCount8 31 = 5
bitCount8 32 = 1
bitCount8 33 = 2
bitCount8 34 = 2
bitCount8 35 = 3
bitCount8 36 = 2
bitCount8 37 = 3
bitCount8 38 = 3
bitCount8 39 = 4
bitCount8 40 = 2
bitCount8 41 = 3
bitCount8 42 = 3
bitCount8 43 = 4
bitCount8 44 = 3
bitCount8 45 = 4
bitCount8 46 = 4
bitCount8 47 = 5
bitCount8 48 = 2
bitCount8 49 = 3
bitCount8 50 = 3
bitCount8 51 = 4
bitCount8 52 = 3
bitCount8 53 = 4
bitCount8 54 = 4
bitCount8 55 = 5
bitCount8 56 = 3
bitCount8 57 = 4
bitCount8 58 = 4
bitCount8 59 = 5
bitCount8 60 = 4
bitCount8 61 = 5
bitCount8 62 = 5
bitCount8 63 = 6
bitCount8 64 = 1
bitCount8 65 = 2
bitCount8 66 = 2
bitCount8 67 = 3
bitCount8 68 = 2
bitCount8 69 = 3
bitCount8 70 = 3
bitCount8 71 = 4
bitCount8 72 = 2
bitCount8 73 = 3
bitCount8 74 = 3
bitCount8 75 = 4
bitCount8 76 = 3
bitCount8 77 = 4
bitCount8 78 = 4
bitCount8 79 = 5
bitCount8 80 = 2
bitCount8 81 = 3
bitCount8 82 = 3
bitCount8 83 = 4
bitCount8 84 = 3
bitCount8 85 = 4
bitCount8 86 = 4
bitCount8 87 = 5
bitCount8 88 = 3
bitCount8 89 = 4
bitCount8 90 = 4
bitCount8 91 = 5
bitCount8 92 = 4
bitCount8 93 = 5
bitCount8 94 = 5
bitCount8 95 = 6
bitCount8 96 = 2
bitCount8 97 = 3
bitCount8 98 = 3
bitCount8 99 = 4
bitCount8 100 = 3
bitCount8 101 = 4
bitCount8 102 = 4
bitCount8 103 = 5
bitCount8 104 = 3
bitCount8 105 = 4
bitCount8 106 = 4
bitCount8 107 = 5
bitCount8 108 = 4
bitCount8 109 = 5
bitCount8 110 = 5
bitCount8 111 = 6
bitCount8 112 = 3
bitCount8 113 = 4
bitCount8 114 = 4
bitCount8 115 = 5
bitCount8 116 = 4
bitCount8 117 = 5
bitCount8 118 = 5
bitCount8 119 = 6
bitCount8 120 = 4
bitCount8 121 = 5
bitCount8 122 = 5
bitCount8 123 = 6
bitCount8 124 = 5
bitCount8 125 = 6
bitCount8 126 = 6
bitCount8 127 = 7
bitCount8 128 = 1
bitCount8 129 = 2
bitCount8 130 = 2
bitCount8 131 = 3
bitCount8 132 = 2
bitCount8 133 = 3
bitCount8 134 = 3
bitCount8 135 = 4
bitCount8 136 = 2
bitCount8 137 = 3
bitCount8 138 = 3
bitCount8 139 = 4
bitCount8 140 = 3
bitCount8 141 = 4
bitCount8 142 = 4
bitCount8 143 = 5
bitCount8 144 = 2
bitCount8 145 = 3
bitCount8 146 = 3
bitCount8 147 = 4
bitCount8 148 = 3
bitCount8 149 = 4
bitCount8 150 = 4
bitCount8 151 = 5
bitCount8 152 = 3
bitCount8 153 = 4
bitCount8 154 = 4
bitCount8 155 = 5
bitCount8 156 = 4
bitCount8 157 = 5
bitCount8 158 = 5
bitCount8 159 = 6
bitCount8 160 = 2
bitCount8 161 = 3
bitCount8 162 = 3
bitCount8 163 = 4
bitCount8 164 = 3
bitCount8 165 = 4
bitCount8 166 = 4
bitCount8 167 = 5
bitCount8 168 = 3
bitCount8 169 = 4
bitCount8 170 = 4
bitCount8 171 = 5
bitCount8 172 = 4
bitCount8 173 = 5
bitCount8 174 = 5
bitCount8 175 = 6
bitCount8 176 = 3
bitCount8 177 = 4
bitCount8 178 = 4
bitCount8 179 = 5
bitCount8 180 = 4
bitCount8 181 = 5
bitCount8 182 = 5
bitCount8 183 = 6
bitCount8 184 = 4
bitCount8 185 = 5
bitCount8 186 = 5
bitCount8 187 = 6
bitCount8 188 = 5
bitCount8 189 = 6
bitCount8 190 = 6
bitCount8 191 = 7
bitCount8 192 = 2
bitCount8 193 = 3
bitCount8 194 = 3
bitCount8 195 = 4
bitCount8 196 = 3
bitCount8 197 = 4
bitCount8 198 = 4
bitCount8 199 = 5
bitCount8 200 = 3
bitCount8 201 = 4
bitCount8 202 = 4
bitCount8 203 = 5
bitCount8 204 = 4
bitCount8 205 = 5
bitCount8 206 = 5
bitCount8 207 = 6
bitCount8 208 = 3
bitCount8 209 = 4
bitCount8 210 = 4
bitCount8 211 = 5
bitCount8 212 = 4
bitCount8 213 = 5
bitCount8 214 = 5
bitCount8 215 = 6
bitCount8 216 = 4
bitCount8 217 = 5
bitCount8 218 = 5
bitCount8 219 = 6
bitCount8 220 = 5
bitCount8 221 = 6
bitCount8 222 = 6
bitCount8 223 = 7
bitCount8 224 = 3
bitCount8 225 = 4
bitCount8 226 = 4
bitCount8 227 = 5
bitCount8 228 = 4
bitCount8 229 = 5
bitCount8 230 = 5
bitCount8 231 = 6
bitCount8 232 = 4
bitCount8 233 = 5
bitCount8 234 = 5
bitCount8 235 = 6
bitCount8 236 = 5
bitCount8 237 = 6
bitCount8 238 = 6
bitCount8 239 = 7
bitCount8 240 = 4
bitCount8 241 = 5
bitCount8 242 = 5
bitCount8 243 = 6
bitCount8 244 = 5
bitCount8 245 = 6
bitCount8 246 = 6
bitCount8 247 = 7
bitCount8 248 = 5
bitCount8 249 = 6
bitCount8 250 = 6
bitCount8 251 = 7
bitCount8 252 = 6
bitCount8 253 = 7
bitCount8 254 = 7
bitCount8 255 = 8
