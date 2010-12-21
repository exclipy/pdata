import Control.Monad
import PVector

main = do forM_ [1..100] (\x ->
            print $ foldl (|>) empty [1..x])
