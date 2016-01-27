{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}

module Statistics.Correlation.Spearman
    ( spearman
    , spearmanMatrix
    ) where

import Data.Ord
import qualified Data.Vector.Unboxed as U
import           Data.Vector.Generic           ((!))
import qualified Data.Vector.Generic         as G
import qualified Data.Vector.Generic.Mutable as M

import qualified Data.Matrix.Unboxed as MU
import qualified Data.Matrix.Generic as MG
import qualified Data.Matrix.Symmetric as MS
import qualified Data.Vector.Algorithms.Intro as I

import Statistics.Correlation.Pearson

-- | compute spearman correlation between two samples
spearman :: ( Ord a
            , Ord b
            , G.Vector v a
            , G.Vector v b
            , G.Vector v (a, b)
            , G.Vector v Int
            , G.Vector v Double
            , G.Vector v (Double, Double)
            , G.Vector v (Int, a)
            , G.Vector v (Int, b)
            )
         => v (a, b)
         -> Double
spearman xy
  = pearson
  $ G.zip (rankUnsorted x) (rankUnsorted y)
  where
    (x, y) = G.unzip xy
{-# INLINE spearman #-}

spearmanMatrix :: ( MG.Matrix m v Double
                  , G.Vector v (Int, Double)
                  , G.Vector v Int
                  , G.Vector v (Double, Double)
                  )
               => m v Double -> MS.SymMatrix U.Vector Double
spearmanMatrix mat = pearsonMatrix mat'
  where
    mat' :: MU.Matrix Double
    mat' = MG.fromRows $ map (rankUnsorted . G.convert) $ MG.toRows mat

-- Private data type for unfolding
data Rank v a = Rank {
      rankCnt :: {-# UNPACK #-} !Int        -- Number of ranks to return
    , rankVal :: {-# UNPACK #-} !Double     -- Rank to return
    , rankNum :: {-# UNPACK #-} !Double     -- Current rank
    , rankVec :: v a                        -- Remaining vector
    }

-- | Calculate rank of every element of sample. In case of ties ranks
--   are averaged. Sample should be already sorted in ascending order.
--
-- >>> rank (==) (fromList [10,20,30::Int])
-- > fromList [1.0,2.0,3.0]
--
-- >>> rank (==) (fromList [10,10,10,30::Int])
-- > fromList [2.0,2.0,2.0,4.0]
rank :: (G.Vector v a, G.Vector v Double)
     => (a -> a -> Bool)        -- ^ Equivalence relation
     -> v a                     -- ^ Vector to rank
     -> v Double
rank eq vec = G.unfoldr go (Rank 0 (-1) 1 vec)
  where
    go (Rank 0 _ r v)
      | G.null v  = Nothing
      | otherwise =
          case G.length h of
            1 -> Just (r, Rank 0 0 (r+1) rest)
            n -> go Rank { rankCnt = n
                         , rankVal = 0.5 * (r*2 + fromIntegral (n-1))
                         , rankNum = r + fromIntegral n
                         , rankVec = rest
                         }
          where
            (h,rest) = G.span (eq $ G.head v) v
    go (Rank n val r v) = Just (val, Rank (n-1) val r v)
{-# INLINE rank #-}

-- | Compute rank of every element of vector. Unlike rank it doesn't
--   require sample to be sorted.
rankUnsorted :: ( Ord a
                , G.Vector v a
                , G.Vector v Int
                , G.Vector v Double
                , G.Vector v (Int, a)
                )
             => v a
             -> v Double
rankUnsorted xs = G.create $ do
    -- Put ranks into their original positions
    -- NOTE: backpermute will do wrong thing
    vec <- M.new n
    for 0 n $ \i ->
      M.unsafeWrite vec (index ! i) (ranks ! i)
    return vec
  where
    n = G.length xs
    -- Calculate ranks for sorted array
    ranks = rank (==) sorted
    -- Sort vector and retain original indices of elements
    (index, sorted)
      = G.unzip
      $ G.modify (I.sortBy (comparing snd))
      $ G.zip (G.enumFromTo 0 (G.length xs - 1)) xs
{-# INLINE rankUnsorted #-}

-- | Simple for loop.  Counts from /start/ to /end/-1.
for :: Monad m => Int -> Int -> (Int -> m ()) -> m ()
for n0 !n f = loop n0
  where
    loop i | i == n    = return ()
           | otherwise = f i >> loop (i+1)
{-# INLINE for #-}
