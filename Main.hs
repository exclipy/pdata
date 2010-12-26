import Control.Monad
import PHashMap
import Data.Word

main = let hashFn = fromIntegral.(`mod` 64)
           indices = [0..32]
           hmaps = zipWith (\hm i -> insert i i hm)
                           ((empty hashFn):hmaps)
                           indices
           values = map (flip PHashMap.lookup (last hmaps)) indices
           in do forM_ hmaps (print.show)
                 print values
