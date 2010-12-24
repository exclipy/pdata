import Control.Monad
import PHashMap
import Data.Word

main = let hashFn x = if even x then 0 else 1
           hmaps = take 32 $ zipWith (\hm i -> update i i hm)
                                     ((empty hashFn):hmaps)
                                     [0, 5, 31, 32, 1]
           in forM_ hmaps (print.show)
