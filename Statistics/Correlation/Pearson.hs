{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
-- |
-- Module      : Statistics.Correlation.Pearson
--

module Statistics.Correlation.Pearson
    ( pearson
    , pearsonMatrix
    ) where

import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed as U

import qualified Data.Matrix.Generic as MG
import qualified Data.Matrix.Symmetric as MS

import Statistics.Correlation.Internal

-- | use Welford's one-pass algorithm to calculate the Pearson's correlation
-- between two samples.
pearson :: G.Vector v (Double, Double) => v (Double, Double) -> Double
pearson xy | n <= 1 = 0/0
           | otherwise = sum_cross / (sqrt sum_xsq * sqrt sum_ysq)
  where
    (sum_cross, sum_xsq, sum_ysq, _, _, _) = G.foldl step (0,0,0,0,0,0::Int) xy
    step (!vxy, !vx, !vy, !mx, !my, !i) (x, y) =
        ( vxy + delta_x * delta_y * ratio
        , vx + delta_x * delta_x * ratio
        , vy + delta_y * delta_y * ratio
        , mx + delta_x / fromIntegral (i+1)
        , my + delta_y / fromIntegral (i+1)
        , i + 1
        )
      where
        delta_x = x - mx
        delta_y = y - my
        ratio = fromIntegral i / fromIntegral (i+1)
    n = G.length xy
{-# INLINE pearson #-}

pearsonMatrix :: (MG.Matrix m v Double, G.Vector v (Double, Double))
              => m v Double -> MS.SymMatrix U.Vector Double
pearsonMatrix = correlationMatrix pearson
{-# INLINE pearsonMatrix #-}
