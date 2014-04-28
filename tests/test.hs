{-# LANGUAGE OverloadedStrings, UnicodeSyntax, TemplateHaskell, BangPatterns #-}
import Test.Tasty

main ∷ IO ()
main = defaultMain tests

tests ∷ TestTree
tests = testsGroup "Tests" 
    [ testKendall ]

testKendall ∷ 
