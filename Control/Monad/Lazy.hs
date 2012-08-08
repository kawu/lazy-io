module Control.Monad.Lazy
( mapM
, forM
, sequence
) where

import Prelude hiding (mapM, sequence)
import System.IO.Unsafe (unsafeInterleaveIO)

sequence (mx:xs) = unsafeInterleaveIO $
    combine xs =<< mx
    where combine xs x = return . (x:) =<< sequence xs
sequence [] = return []

mapM f (x:xs) = unsafeInterleaveIO $ do
    y <- f x
    ys <- mapM f xs
    return (y : ys)
mapM _ [] = return []

forM = flip mapM
