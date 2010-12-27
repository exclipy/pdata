import Control.Monad
import PHashMap
import Data.Word

main = let hashFn = fromIntegral.(`mod` 64)
           indices = [0,64,128,64]
           hmaps = zipWith (\hm i -> insert i i hm)
                           ((empty hashFn):hmaps)
                           indices
           hmap = last hmaps
           values = map (flip PHashMap.lookup hmap) indices
           dindices = indices
           dmaps = zipWith (\hm i -> delete i hm)
                           (hmap:dmaps)
                           dindices
           in do forM_ hmaps (print.show)
                 print values
                 forM_ dmaps (print.show)
