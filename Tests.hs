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
  , testGroup "G1 grammar from ldta" [
     testCase "a^20 b^150 a OK" test_g1
     ]
  , testGroup "Another test grammar" gr_tests
  , testGroup "Simple recurisve grammar" rec_tests
  ]

-- SAB grammar from ldta paper
sab =
    [ ("S", [[Nonterminal "A", Nonterminal "S", Terminal 'd']
            ,[Nonterminal "B", Nonterminal "S"]
            ,[]
            ])
    , ("A", [[Terminal 'a'], [Terminal 'c']])
    , ("B", [[Terminal 'a'], [Terminal 'b']])
    ]
test_sab input expected = parse sab input @?= expected

-- Grammar Г1 from ldta paper
g1 =
    [ ("S", [[Nonterminal "C", Terminal 'a']
            ,[Terminal 'd']
            ])
    , ("B",  [[]
             ,[Terminal 'a']])
    , ("C", [[Terminal 'b']
            ,[Nonterminal "B",Nonterminal "C", Terminal 'b']
            ,[Terminal 'b', Terminal 'b']])
    ]
test_g1 = parse g1 (concat [replicate 20 'a', replicate 150 'b', "a"]) @?= True

-- Grammar Г2 from ldta paper
g2 =
    [ ("S", [[Terminal 'b']
            ,[Nonterminal "S", Nonterminal "S"]
            ,[Nonterminal "S", Nonterminal "S", Nonterminal "S"]
            ])
    ]
test_g2 n = parse g2 (replicate n 'b') @?= True

-- Grammar Г2* from ldta paper
g2star =
    [ ("S", [[Terminal 'b']
            ,[Nonterminal "S", Nonterminal "S", Nonterminal "A"]]),
      ("A", [[Nonterminal "S"]
            ,[]])
    ]
test_g2star n = parse g2star (replicate n 'b') @?= True

-- another grammar
gr =
    [ ("S", [[Nonterminal "A", Terminal 'b']
            ,[Nonterminal "A", Terminal 'c']
            ,[Nonterminal "A"]
            ])
    , ("A",  [[Nonterminal "A1"]])
    , ("A1", [[Nonterminal "A2"]])
    , ("A2", [[Terminal 'a']])
    ]

gr_tests = flip map testData $
    \(str, res) -> testCase
        ("parse gr \""++str++"\" is " ++ show res)
        (parse gr str @?= res)
    where testData = [("",False),("a",True),("b",False),("ab",True),("c",False),("ac",True),("abc",False)]

-- simple recursive grammar with epsilon-production
rec = [ ("S", [[Nonterminal "S", Terminal 'a'], []]) ]
rec_tests = flip map testData $
    \(str, res) -> testCase
        ("parse rec \""++str++"\" is " ++ show res)
        (parse rec str @?= res)
    where
    testData =
        [(replicate n 'a', True) | n <- [0..10]] ++
        [(s,False) | s <- ["ab","ba","b","aab"]]
