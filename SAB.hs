-- (комментарии пока пишу по-русски, т.к. так проще. Потом все вычистим, перепишем)
-- Парсер грамматики из ldta:
--   S ::= A S d | B S | epsilon
--   A ::= a | c
--   B ::= a | b

module SAB (main) where

import GSS
import Text.Printf

-- import Data.Map as M
import Data.Set as S
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
  show (PS gss_ i inp) = printf "{{{STATE: U=%s; c_u=%s; R=%s; input[i]=%s }}}" (show $ yu gss_) (show $ curr_u gss_) (show $ er gss_) (take 10 (drop i inp))

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
  
popCurrent :: Parser ()
popCurrent = do
  oldgss <- gets gss
  i <- gets curr_i
  let newgs = pop (curr_u oldgss) i oldgss
  modify (\s -> s{gss=newgs})

incr = modify (\s -> s{curr_i = (curr_i s) + 1})

logState mark = do
  tell mark
  tell ". "
  ps <- get
  tellLn $ show ps
--- Переложение стр. 119 ldta:
mkPS inp = PS { gss = mkGState l_0, curr_i = 0, input = inp }

parse = do
  logState "parse"
  goto l_s

l_s = L "s" $ do
  inp <- gets input
  i <- gets curr_i
  g <- gets gss  

  when (inp!!i `elem` "ac") $ addToCurrent l_s1
  when (inp!!i `elem` "ab") $ addToCurrent l_s2
  when (inp!!i `elem` "d$") $ addToCurrent l_s3
  
  logState "l_s"
  goto l_0
  

l_0 = L "0" $ do
  gss_ <- gets gss
  inp <- gets input
  let r = er gss_
  let m = length inp + 1
      
  logState "l_0"
  if not (Prelude.null r) 
    then do let (label, u, i) = last r
            tellLn $ printf "  L=%s; u=%s; i=%d" (show label) (show u) i
            modify (\s -> s{gss = gss_{curr_u = u, er = init r}, curr_i = i})
            goto label
            
    else if (l_0, Root,m) `S.member` (yu gss_)
         then return Success
         else do tell "U is: "
                 tellLn (show $ yu gss_)
                 return Failure
    
l_s1 = L "s1" $ do
  createAtCurrent l_1
  logState "l_s1"
  goto l_a
  
l_1 = L "1" $ do
  tellLn "l_1"
  createAtCurrent l_2
  goto l_s
  
l_2 = L "2" $ do
  (PS gss_ i inp) <- get
  when (inp!!i == 'd') $ popCurrent
  goto l_0
         
l_s2 = L "s2" $ do
  createAtCurrent l_3
  goto l_b
  
l_3 = L "3" $ do
  createAtCurrent l_4
  goto l_s
  
l_4 = L "4" $ do
  popCurrent
  goto l_0
  
l_s3 = L "s3" $ do
  popCurrent
  goto l_0
  
l_a = L "a" $ do
  (PS gss_ i inp) <- get
  if (inp!!i `elem` "ac") 
    then do incr
            popCurrent
            tell "branch 1. "; logState "l_a"
            goto l_0
    else do tell "branch 2. "; logState "l_a"
            goto l_0

l_b = L "b" $ do
  (PS gss_ i inp) <- get
  if (inp!!i `elem` "ab") 
    then do incr
            popCurrent
            goto l_0
    else goto l_0

main = do
  let (ret, log) = evalRWS parse () (mkPS "aad$")
  putStr "Result is "; print ret
  putStrLn "\nLOG:"
  putStrLn log
  

