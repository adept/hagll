import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Test.QuickCheck
import Test.HUnit

import Data.List

import Grammar

main = defaultMain tests

tests = [
  testGroup "SAB grammar from ldta" [
     testCase "aad OK" (test_sab "aad" True),
     testCase "acd OK" (test_sab "acd" True),
     testCase "add Fails" (test_sab "add" False)
     ]
  ]

-- SAB grammar
sab =
    [ ("S", [[Nonterminal "A", Nonterminal "S", Terminal 'd']
            ,[Nonterminal "B", Nonterminal "S"]
            ,[]
            ])
    , ("A", [[Terminal 'a'], [Terminal 'c']])
    , ("B", [[Terminal 'a'], [Terminal 'b']])
    ]
test_sab input expected = parse sab input @?= expected
