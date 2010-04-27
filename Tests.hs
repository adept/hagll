import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Test.QuickCheck
import Test.HUnit

import Data.List

import Grammar

main = defaultMain tests

tests = [
  testGroup "Grammars from ldta" [
     testCase "SAB: aad OK" (test_sab "aad" True),
     testCase "SAB: acd OK" (test_sab "acd" True),
     testCase "SAB: add Fails" (test_sab "add" False),
     testCase "Г1:  (a^20)(b^150)(a) OK" test_g1
     ]
  , testGroup "Slooooooow tests (from ldta)" [
     testCase "Г2*: (b^50) OK" (test_g2star 50)
     , testCase "Г2:  (b^20) OK" (test_g2 20)
     ]
  , testGroup "Grammar from GLR parsing in Haskell" [
    testCase "(b^20) is OK" (test_glr 20),
    testCase "(b^25) is OK" (test_glr 25)
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

-- From "Generalized LR parsing in Haskell"
-- S : T {}
-- T : A 'b' {} | T T T {}
-- A : T 'b' A A A {} | T T 'b' {} | {}
glr = [ ("S", [[Nonterminal "T"]]),
        ("T", [[Nonterminal "A", Terminal 'b']
              ,[Nonterminal "T", Nonterminal "T", Nonterminal "T"]]),
        ("A", [[Nonterminal "T", Terminal 'b', Nonterminal "A", Nonterminal "A", Nonterminal "A"]
              ,[Nonterminal "T", Nonterminal "T", Terminal 'b']
              ,[]])
      ]
test_glr n = parse glr (replicate n 'b') @?= True
