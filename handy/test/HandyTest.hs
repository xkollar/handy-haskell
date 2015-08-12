module Main ( main ) where

import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)

-- import Test.QuickCheck
import Test.HUnit hiding (Test)

import Handy.List

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests =
    [ testGroup "Handy.List"
        [ testCase "Contains first" test_ordElem_1
        , testCase "Contains middle" test_ordElem_2
        , testCase "Contains last" test_ordElem_3
        , testCase "Does not contains before" test_ordElem_4
        , testCase "Does not contains middle" test_ordElem_5
        , testCase "Does not contains after" test_ordElem_6
        , testCase "Contains infinite" test_ordElem_7
        , testCase "Does not contains infinite" test_ordElem_8
        ]
    , testGroup "Trivial"
        [ testCase "test" test_trivial_1
        , testProperty "prop" prop_trivial_1
        ]
    ]

test_trivial_1 :: Assertion
test_trivial_1 = () @?= ()

prop_trivial_1 :: () -> Bool
prop_trivial_1 x = x == x

holedFiniteList :: [Int]
holedFiniteList = [100,102..200]

test_ordElem_1 :: Assertion
test_ordElem_1 = assertBool "begin" $ 100 `ordElem` holedFiniteList where

test_ordElem_2 :: Assertion
test_ordElem_2 = assertBool "middle" $ 150 `ordElem` holedFiniteList where

test_ordElem_3 :: Assertion
test_ordElem_3 = assertBool "end" $ 200 `ordElem` holedFiniteList where

test_ordElem_4 :: Assertion
test_ordElem_4 = assertBool "before" . not $ 1 `ordElem` holedFiniteList where

test_ordElem_5 :: Assertion
test_ordElem_5 = assertBool "between" . not $ 151 `ordElem` holedFiniteList where

test_ordElem_6 :: Assertion
test_ordElem_6 = assertBool "after" . not $ 300 `ordElem` holedFiniteList where

holedInfiniteList :: [Int]
holedInfiniteList = [1,3..]

test_ordElem_7 :: Assertion
test_ordElem_7 = assertBool "infinite contains" $ 1001 `ordElem` holedInfiniteList

test_ordElem_8 :: Assertion
test_ordElem_8 = assertBool "infinite does not contain" . not $ 1000 `ordElem` holedInfiniteList
