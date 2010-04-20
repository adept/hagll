module GSS
 ( GState, create, add )
where

import Data.Map as M
import Data.Set as S

type Pos = Int
type Node lab = (lab, Pos)
type R lab = [(lab, Node lab, Pos)]
type U lab = Map Pos [(lab,Node lab)]
type P lab = Set (Node lab, Pos)
type G lab = Set (Node lab)
type E lab = Map (Node lab) (Set (Node lab)) -- parents

data GState lab = GState
    { gee :: G lab
    , er  :: R lab
    , pe  :: P lab
    , curr_u :: Node lab
    , parents :: E lab
    , yu  :: U lab
    }

create :: (Eq lab, Ord lab) => lab -> Node lab -> Pos -> GState lab
       -> (GState lab, Node lab)
create label u i oldgs =
    if v `S.member` g
    then
        (oldgs, v)
    else
        (add_popped.connect_v.insert_v $ oldgs, v)
    where
    g = gee oldgs
    p = pe oldgs
    v = (label, i)
    insert_v gstate = gstate { gee = S.insert v g }
    connect_v gstate = gstate { parents = M.insertWith (S.union) v (S.singleton u) (parents gstate) }
    add_popped gstate = foldl (\gs j -> add label u j gs) gstate [ j | (x,j) <- S.elems p, x == v ]

add :: (Eq lab) => lab -> Node lab -> Pos -> GState lab -> GState lab
add label u i oldgs =
    if not ((label,u) `elem` u_i)
        then oldgs {er = (label, u, i):r, yu = M.insertWith (++) i [(label,u)] yu_}
        else oldgs
    where
    r = er oldgs
    yu_ = yu oldgs
    u_i = maybe [] id $ M.lookup i yu_

{-
pop u i = do
  let (label, _) = u
  prnts <- gets parents
  modify (\s -> s{pe = S.insert (u,i) (pe s)})
  p <- gets pe
  tellLn $ "  P is now " ++ show p
  if u `M.member` prnts
    then do tellLn $ "  Has parents: " ++ show prnts
            forM_ (S.elems (prnts!u)) $ \v -> add label v i
    else return ()
-}
