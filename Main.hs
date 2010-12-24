import Control.Monad
import PHashMap
import Data.Word

main = let hashFn = fromIntegral.(`mod` 64)
           indices = [0, 5, 31, 32, 1, 64]
           hmaps = take 32 $ zipWith (\hm i -> update i i hm)
                                     ((empty hashFn):hmaps)
                                     indices
           values = map (index (last hmaps)) indices
           in do forM_ hmaps (print.show)
                 print values
