-- | The module is intended to be imported qualified:
--
-- >>> import qualified Control.Monad.LazyIO as LazyIO


module Control.Monad.LazyIO
( sequence
, mapM
, forM
) where


import System.IO.Unsafe (unsafeInterleaveIO)
import Prelude hiding (mapM, sequence)


-- | Lazily evaluate each action in the sequence from left to right,
-- and collect the results.
sequence :: [IO a] -> IO [a]
sequence (mx:mxs) = do
    x   <- mx
    xs  <- unsafeInterleaveIO (sequence mxs)
    return (x : xs)
sequence [] = return []


-- | `mapM` f is equivalent to `sequence` . `map` f.
mapM :: (a -> IO b) -> [a] -> IO [b]
mapM f = sequence . map f


-- | `forM` is `mapM` with its arguments flipped.
forM :: [a] -> (a -> IO b) -> IO [b]
forM = flip mapM
