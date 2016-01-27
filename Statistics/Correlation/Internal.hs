{-# LANGUAGE FlexibleContexts #-}
module Statistics.Correlation.Internal
    ( correlationMatrix ) where

import Control.Monad (forM_)
import qualified Data.Matrix.Generic as MG
import qualified Data.Matrix.Generic.Mutable as MGM
import qualified Data.Matrix.Symmetric as MS

import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed as U

-- | Compute correlation between rows of a matrix.
correlationMatrix :: (MG.Matrix m v a, G.Vector v a, G.Vector v (a, a))
                  => (v (a, a) -> Double)
                  -> m v a
                  -> MS.SymMatrix U.Vector Double
correlationMatrix cor xs = MG.create $ do
    mat <- MGM.new (n,n)
    forM_ [0..n-1] $ \i -> forM_ [i..n-1] $ \j -> if i == j
        then MGM.unsafeWrite mat (i,j) 1.0
        else MGM.unsafeWrite mat (i,j) $ cor $ G.zip (xs `MG.takeRow` i) $
            xs `MG.takeRow` j
    return mat
  where
    (n,_) = MG.dim xs
{-# INLINE correlationMatrix #-}
