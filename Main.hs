import Control.Monad
import PHashMap
import Data.Word

main = let hashFn = fromIntegral.(`mod` 64)
           indices = [0..26]
           hmaps = zipWith (\hm i -> insert i i hm)
                           ((empty hashFn):hmaps)
                           indices
           values = map (flip PHashMap.lookup (last hmaps)) indices
           deleted = map (flip PHashMap.delete (last hmaps)) indices
           in do forM_ hmaps (print.show)
                 print values
                 forM_ deleted (print.show)
