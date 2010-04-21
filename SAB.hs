-- (комментарии пока пишу по-русски, т.к. так проще. Потом все вычистим, перепишем)
-- Парсер грамматики из ldta:
--   S ::= A S d | B S | epsilon
--   A ::= a | c
--   B ::= a | b

module SAB (parse, Result(..)) where

import GSS
import Text.Printf

import Data.Map as M hiding (fromList)
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

data Result = Success | Failure deriving (Eq,Show)

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
  
correct mark yu_ cu_ r_ inp_ = do  
  (PS gss_ i inp) <- get  
  when ( (drop i inp == inp_) && cu_ == curr_u gss_ && r_ == er gss_ && yu_ == yu gss_ ) $ do 
    tellLn $ ">>>>> CORRECT TILL " ++ mark
  
dumpGraph = do
  gss_ <- gets gss
  tellLn "digraph GSS {"
  sequence_ [ tellLn (printf "%s -> %s;" (show_ a) (show_ b)) | (a,bs) <- M.assocs (parents gss_), b <- S.elems bs ]
  tellLn "}"
  where
    show_ a = Prelude.map toUnderscore $ show a
    toUnderscore ' ' = '_'
    toUnderscore x = x
    
--- Переложение стр. 119 ldta:
mkPS inp = PS { gss = mkGState l_0, curr_i = 0, input = inp }

l_s = L "s" $ do
  inp <- gets input
  i <- gets curr_i

  when (inp!!i `elem` "ac") $ addToCurrent l_s1
  when (inp!!i `elem` "ab") $ addToCurrent l_s2
  when (inp!!i `elem` "d$") $ addToCurrent l_s3
  
  logState "l_s"
  goto l_0
  

l_0 = L "0" $ do
  gss_ <- gets gss
  inp <- gets input
  let r = er gss_
  let m = length inp - 1
      
  logState "l_0"
  correct "p.118, Similarly, processing (L3, [L00], 1) gives ..."
    (fromList [(l_1,Node l_0 0,1),(l_3,Node l_0 0,1),(l_s1,Node l_0 0,0),(l_s1,Node l_2 1,1),(l_s1,Node l_4 1,1),(l_s2,Node l_0 0,0),(l_s2,Node l_2 1,1),(l_s2,Node l_4 1,1)]) -- U
    (Node l_4 1) -- c_u
    [(l_s2,Node l_4 1,1),(l_s1,Node l_4 1,1),(l_s2,Node l_2 1,1),(l_s1,Node l_2 1,1)] -- R
    "ad$" -- inp
    
  correct "p. 118, Processing each of these elements in turn results in ..." 
    (fromList [(l_1,Node l_0 0,1),(l_1,Node l_2 1,2),(l_1,Node l_4 1,2),(l_3,Node l_0 0,1),(l_3,Node l_2 1,2),(l_3,Node l_4 1,2),(l_s1,Node l_0 0,0),(l_s1,Node l_2 1,1),(l_s1,Node l_4 1,1),(l_s2,Node l_0 0,0),(l_s2,Node l_2 1,1),(l_s2,Node l_4 1,1)])
    (Node l_3 1)
    [(l_3,Node l_4 1,2),(l_1,Node l_4 1,2),(l_3,Node l_2 1,2),(l_1,Node l_2 1,2)]
    "d$"    
  
  correct "p. 118, Then, as I[2] = d, processing each of these results in ..." 
    (fromList [(l_1,Node l_0 0,1),(l_1,Node l_2 1,2),(l_1,Node l_4 1,2),(l_3,Node l_0 0,1),(l_3,Node l_2 1,2),(l_3,Node l_4 1,2),(l_s1,Node l_0 0,0),(l_s1,Node l_2 1,1),(l_s1,Node l_4 1,1),(l_s2,Node l_0 0,0),(l_s2,Node l_2 1,1),(l_s2,Node l_4 1,1),(l_s3,Node l_2 2,2),(l_s3,Node l_4 2,2)]) 
    (Node l_4 2)
    [(l_s3,Node l_4 2,2),(l_s3,Node l_2 2,2)]
    "d$"

  correct "p. 118, From this set we get..." 
    (fromList [(l_1,Node l_0 0,1),(l_1,Node l_2 1,2),(l_1,Node l_4 1,2),(l_2,Node l_2 1,2),(l_2,Node l_4 1,2),(l_3,Node l_0 0,1),(l_3,Node l_2 1,2),(l_3,Node l_4 1,2),(l_4,Node l_2 1,2),(l_4,Node l_4 1,2),(l_s1,Node l_0 0,0),(l_s1,Node l_2 1,1),(l_s1,Node l_4 1,1),(l_s2,Node l_0 0,0),(l_s2,Node l_2 1,1),(l_s2,Node l_4 1,1),(l_s3,Node l_2 2,2),(l_s3,Node l_4 2,2)]) 
    (Node l_4 2)
    [(l_4,Node l_4 1,2),(l_4,Node l_2 1,2),(l_2,Node l_4 1,2),(l_2,Node l_2 1,2)] 
    "d$"

  if not (Prelude.null r) 
    then do let (label, u, i) = last r
            tellLn $ printf "  L=%s; u=%s; i=%d" (show label) (show u) i
            modify (\s -> s{gss = gss_{curr_u = u, er = init r}, curr_i = i})
            goto label
            
    else if (l_0, Root,m) `S.member` (yu gss_)
         then do dumpGraph
                 return Success
         else do tell "U is: "
                 tellLn (show $ yu gss_)
                 return Failure
    
l_s1 = L "s1" $ do
  createAtCurrent l_1
  logState "l_s1"
  goto l_a
  
l_1 = L "1" $ do
  createAtCurrent l_2
  logState "l_1"
  goto l_s
  
l_2 = L "2" $ do
  (PS gss_ i inp) <- get
  when (inp!!i == 'd') $ do incr
                            popCurrent
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

-- Driver(s)
  
testParse showLog input expected = do
  let (ret, log) = evalRWS (goto l_s) () (mkPS $ input++"$")
  when showLog $ do putStrLn "LOG:"
                    putStrLn log
                    putStrLn ""
  putStrLn $ printf "For input %s result is %s" input (show ret)
  when (ret /= expected) $ error $ printf "Error: expected %s" (show expected)

parse input = ret
  where (ret, _) = evalRWS (goto l_s) () (mkPS $ input++"$")
