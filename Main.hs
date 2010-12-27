import Control.Monad
import PHashMap

main = let hashFn = (`mod` 64)
           lmap = [(0,1), (64, 2), (32,3)]
           indices = map fst lmap
           hmap = fromList hashFn lmap
           values = map (flip PHashMap.lookup hmap) indices
           dindices = indices
           dmaps = zipWith (\hm i -> delete i hm)
                           (hmap:dmaps)
                           dindices
           in do print $ show hmap
                 print values
                 forM_ dmaps (print.show)
