module Scratch where

import Data.Map as M
import Data.Set as S
import Control.Monad.RWS
import Data.Maybe
import Debug.Trace

type Input = String -- need to track pos Map Pos Char
type Pos = Int
type LabelId = Int
data Label = L { ident::LabelId, f::Parser Result }
instance Eq Label where
  (L id1 _) == (L id2 _) = id1 == id2
instance Ord Label where
  compare (L id1 _) (L id2 _) = compare id1 id2
instance Show Label where
  show (L id1 _) = "L " ++ show id1
type Node = (Label, Pos)
type R = [(Label, Node, Pos)]
type U = Map Pos [(Label,Node)]
type P = Set (Node, Pos)
type G = Set Node
type E = Map Node (Set Node) -- parents

data ParserState = PS { gee :: G, pee :: P, er::R, pe::P, curr_i :: Pos, curr_u :: Node, input::Input, parents :: E, yu::U } deriving Show
mkPS inp = PS S.empty S.empty [] S.empty 0 node_0 inp M.empty M.empty
type Parser a = RWS () String ParserState a
tellLn s = tell $ s ++ "\n"

data Result = Success | Failure deriving Show

create :: Label -> Node -> Pos -> Parser Node
create label u i = do
  tellLn "create"
  g <- gets gee
  p <- gets pee
  if not ((label, i) `S.member` g)
    then do let v = (label,i)
            modify (\s -> s{parents = M.insertWith (S.union) v (S.singleton u) (parents s)})
            modify (\s -> s{gee = S.insert v g})
            forM_ [ j | (x,j) <- S.elems p, x == v ] $ \j -> add label u j
            return v
    else return (label, i) -- CHECK

add :: Label -> Node -> Pos -> Parser ()
add label u i = do
  tellLn $ "add " ++ show (label,u,i)
  r <- gets er
  yu_ <- gets yu
  let u_i = maybe [] id $ M.lookup i yu_
  if not ((label,u) `elem` u_i)
     then modify (\s -> s{er = (label, u, i):(er s), yu = M.insertWith (++) i [(label,u)] yu_})
     else return ()

pop u i = do
  tellLn $ "pop" ++ show (u,i)
  let (label, _) = u
  pts <- gets parents
  modify (\s -> s{pe = S.insert (u,i) (pe s)})
  if u `M.member` pts 
    then forM_ (S.elems (pts!u)) $ \v -> add label v i
    else return ()
  
l_666 = L 666 undefined
node_0 = (l_666, (-1))
parse = do
  tellLn "parse"
  add l_s node_0 0
  goto l_0
  
goto (L id f) = tellLn ("goto " ++ show id) >> f 

l_0 = L 0 $ do
  r <- gets er
  c_i <- gets curr_i
  inp <- gets input
  tellLn "l_0"
  if not (Prelude.null r) 
    then do let (label, c_u, c_i) = head r
            tellLn $ "  l_0, R is " ++ show r
            modify (\s -> s{er = tail r, curr_u = c_u, curr_i = c_i})
            goto label
            
    else if inp!!c_i == '$'
         then return Success
         else return Failure
              
l_s = L 5 $ do
  tellLn "l_s"
  c_i <- gets curr_i
  c_u <- gets curr_u
  inp <- gets input
  if inp!!c_i == 'a'
    then do tellLn "l_s, case 1"
            add l_s0 c_u c_i
            add l_s1 c_u c_i
            goto l_0
    else if inp!!c_i == '$'
            then do tellLn "l_s, case 2"
                    add l_s1 c_u c_i
                    goto l_0
            else tellLn "l_s, case 3" >> goto l_0
                             
l_s0 = L 50 $ do
  tellLn "l_s0"
  c_i <- gets curr_i
  c_u <- gets curr_u
  inp <- gets input
  if inp!!c_i == 'a'
     then do c_u <- create l_s0' c_u (c_i + 1)
             modify (\s -> s{curr_u = c_u})
             goto l_s
     else goto l_0
          
l_s0' = L 501 $ do
  tellLn "l_s0'"
  c_i <- gets curr_i
  c_u <- gets curr_u
  inp <- gets input
  pop c_u c_i
  goto l_0
  
l_s1 = L 51 $ do
  tellLn "l_s1"
  c_i <- gets curr_i
  c_u <- gets curr_u
  inp <- gets input
  if inp!!c_i == 'a'
     then do pop c_u (c_i+1)
             goto l_0
     else goto l_0

l_s2 = L 52 $ do
  tellLn "l_s2"
  c_i <- gets curr_i
  c_u <- gets curr_u
  inp <- gets input
  if inp!!c_i == '$'
    then do pop c_u c_i
            goto l_0
    else goto l_0
         
main = do
  let (ret, log) = evalRWS parse () (mkPS "a$")
  print ret
  putStrLn log