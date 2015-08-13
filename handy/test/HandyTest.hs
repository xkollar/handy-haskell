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
        [ testGroup "ordElem"
            [ testCaseSimple "Contains first" test_ordElem_1
            , testCaseSimple "Contains middle" test_ordElem_2
            , testCaseSimple "Contains last" test_ordElem_3
            , testCaseSimple "Does not contain before" test_ordElem_4
            , testCaseSimple "Does not contain middle" test_ordElem_5
            , testCaseSimple "Does not contain after" test_ordElem_6
            , testCaseSimple "Contains infinite" test_ordElem_7
            , testCaseSimple "Does not contain infinite" test_ordElem_8
            ]
        ]
    , testGroup "Trivial"
        [ testCase "test" test_trivial_1
        , testProperty "prop" prop_trivial_1
        ]
    ]

testCaseSimple :: AssertionPredicable t => String -> t -> Test
testCaseSimple msg b = testCase msg (b @? msg)

test_trivial_1 :: Assertion
test_trivial_1 = () @?= ()

prop_trivial_1 :: () -> Bool
prop_trivial_1 x = x == x

holedFiniteList :: [Int]
holedFiniteList = [100,102..200]

test_ordElem_1 :: Bool
test_ordElem_1 = 100 `ordElem` holedFiniteList where

test_ordElem_2 :: Bool
test_ordElem_2 = 150 `ordElem` holedFiniteList where

test_ordElem_3 :: Bool
test_ordElem_3 = 200 `ordElem` holedFiniteList where

test_ordElem_4 :: Bool
test_ordElem_4 = not $ 1 `ordElem` holedFiniteList where

test_ordElem_5 :: Bool
test_ordElem_5 = not $ 151 `ordElem` holedFiniteList where

test_ordElem_6 :: Bool
test_ordElem_6 = not $ 300 `ordElem` holedFiniteList where

holedInfiniteList :: [Integer]
holedInfiniteList = [1,3..]

test_ordElem_7 :: Bool
test_ordElem_7 = 1001 `ordElem` holedInfiniteList

test_ordElem_8 :: Bool
test_ordElem_8 = not $ 1000 `ordElem` holedInfiniteList
