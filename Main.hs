import Control.Monad
import PHashMap
import Data.Word

main = let hashFn = fromIntegral.(`mod` 64)
           indices = [0..20]
           hmaps = zipWith (\hm i -> insert i i hm)
                           ((empty hashFn):hmaps)
                           indices
           hmap = last hmaps
           values = map (flip PHashMap.lookup hmap) indices
           dindices = [0..20]
           dmaps = zipWith (\hm i -> delete i hm)
                           (hmap:dmaps)
                           dindices
           in do forM_ hmaps (print.show)
                 print values
                 forM_ dmaps (print.show)
