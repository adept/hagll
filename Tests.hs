import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Test.QuickCheck
import Test.HUnit

import Data.List

import qualified SAB

main = defaultMain tests

tests = [
  testGroup "SAB grammar from ldta" [
     testCase "aad OK" (test_sab "aad" SAB.Success),
     testCase "acd OK" (test_sab "acd" SAB.Success),
     testCase "add Fails" (test_sab "add" SAB.Failure)
     ]
  ]

-- SAB grammar
test_sab input expected = SAB.parse input @?= expected
