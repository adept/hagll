import Grammar
{-
 - S = A | B
 - B = b | ob
 - A = a | oa
 -}
g :: Grammar
g = [ ("S", [[Nonterminal "A"], [Nonterminal "B"]])
    , ("B", [[Terminal 'b'], [Terminal 'o', Terminal 'b']])
    , ("A", [[Terminal 'a'], [Terminal 'o', Terminal 'a']])
    ]
g2 :: Grammar
g2 = [ ("S", [[Nonterminal "A",Nonterminal "A"], [Nonterminal "B",Nonterminal "A"]])
     , ("B", [[Terminal 'b'], [Terminal 'o', Terminal 'b']])
     , ("A", [[Nonterminal "B"], [Terminal 'a'], [Terminal 'o', Terminal 'a']])
     ]
sab =
    [ ("S", [[Nonterminal "A", Nonterminal "S", Terminal 'd']
            ,[Nonterminal "B", Nonterminal "S"]
            ,[]
            ])
    , ("A", [[Terminal 'a'], [Terminal 'c']])
    , ("B", [[Terminal 'a'], [Terminal 'b']])
    ]
main = print $ parse sab "aad"
