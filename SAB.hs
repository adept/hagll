-- (комментарии пока пишу по-русски, т.к. так проще. Потом все вычистим, перепишем)
-- Парсер грамматики из ldta:
--   S ::= A S d | B S | epsilon
--   A ::= a | c
--   B ::= a |b

module SAB (main) where

import GSS

import Data.Map as M
-- import Data.Set as S
import Control.Monad.RWS
import Data.Maybe

type Input = String
type Pos = Int
type LabelId = String

data Label = L { ident::LabelId, f::Parser Result }
instance Eq Label where
  (L id1 _) == (L id2 _) = id1 == id2
instance Ord Label where
  compare (L id1 _) (L id2 _) = compare id1 id2
instance Show Label where
  show (L id1 _) = "L_" ++ id1

data ParserState = PS { gss :: GState Label, curr_i :: Pos, input::Input}
instance Show ParserState where
  show (PS _ u inp) = "input[i]=" ++ (take 10 inp)

type Parser a = RWS () String ParserState a

tellLn s = tell $ s ++ "\n"

data Result = Success | Failure deriving Show

goto (L id f) = tellLn ("goto L_" ++ id) >> f 

addToCurrent :: Label -> Parser ()
addToCurrent label = do
  oldgs <- gets gss
  i <- gets curr_i
  let newgs = add label (curr_u oldgs) i oldgs
  modify (\s -> s { gss = newgs}  )

createAtCurrent :: Label -> Parser ()
createAtCurrent label = do
  oldgs <- gets gss
  i <- gets curr_i
  let (newgs, new_u) = create label (curr_u oldgs) i oldgs
  modify (\s -> s { gss = newgs {curr_u = new_u}})
  
--- Переложение стр. 119 ldta:
mkPS inp = PS { gss = mkGState l_0, curr_i = 0, input = inp }

parse = do
  tellLn "parse"
  goto l_s

l_s = L "s" $ do
  inp <- gets input
  i <- gets curr_i
  g <- gets gss  

  when (inp!!i `elem` "ac") $ addToCurrent l_s1
  when (inp!!i `elem` "ab") $ addToCurrent l_s2
  when (inp!!i `elem` "d$") $ addToCurrent l_s3
  
  goto l_0
  

l_0 = L "0" $ do
  gss_ <- gets gss
  let r = er gss_
  
  tellLn "l_0"
  if not (Prelude.null r) 
    then do let (label, u, i) = head r
            tellLn $ "  l_0, R is " ++ show r
            modify (\s -> s{gss = gss_{curr_u = u, er = tail r}, curr_i = i})
            goto label
            
    else if (l_0, root_node) `elem` (yu gss_)!m
         then return Success
         else return Failure
              
  where
    root_node = undefined
    m = undefined -- length of input + 1
    
l_s1 = L "s1" $ do
  tellLn "l_s1"
  createAtCurrent l_1
  goto l_a
  
l_1 = L "1" $ do
  tellLn "l_1"
  createAtCurrent l_2
  goto l_s
  
l_2 = L "2" $ undefined
  -- TODO
         
l_s2 = undefined
l_s3 = undefined
l_a = undefined

main = do
  let (ret, log) = evalRWS parse () (mkPS "aaad$")
  print ret
  putStrLn log
  

