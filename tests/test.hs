{-# LANGUAGE OverloadedStrings, UnicodeSyntax, BangPatterns #-}
import Test.Tasty
import qualified Data.Vector.Unboxed as V
import Test.Tasty.Golden
import Statistics.Correlation.Kendall
import Data.List.Split
import Text.Printf
import qualified Data.ByteString.Lazy.Char8 as B

main ∷ IO ()
main = defaultMain tests

tests ∷ TestTree
tests = testGroup "Tests" 
    [ goldenVsString "Kendall's tau" "tests/result.txt" testKendall ]

testKendall ∷ IO B.ByteString
testKendall = do
    xy ← readData
    return.B.unlines.map (B.pack.printf "%0.5f".kendall) $ xy

readData ∷ IO [V.Vector (Double, Double)]
readData = do
    d ← readFile "tests/data.txt"
    return.map f.chunksOf 2.lines $ d
  where
      f [a, b] = V.fromList $ zip ((map read.words) a) ((map read.words) b)
