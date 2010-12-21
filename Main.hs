import Control.Monad
import PVector

main = do forM_ [0..69] (\x ->
            print $ update x 0 $ foldl (|>) empty [1..70])
