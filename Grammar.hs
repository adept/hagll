module Grammar (parse,Symbol(..),Grammar,Production) where
import Control.Monad.State
import GSS (GState)
import qualified GSS
import qualified Data.Map as M
import Data.List

-- BNF
data Symbol = Terminal Char | Nonterminal String
    deriving Show
data LSymbol = LTerminal Char
             | LNonterminal Int String
    deriving Show
type Str = [LSymbol]
type Production = (String, [[Symbol]])
type Grammar = [Production]
type LGrammar = M.Map String [(Int,Str)]
data L = L { ident :: LabelId, goto :: ParseM Bool }
instance Eq L where
  (L id1 _) == (L id2 _) = id1 == id2
instance Ord L where
  compare (L id1 _) (L id2 _) = compare id1 id2
instance Show L where
  show (L id1 _) = show id1

-- LabelId is combination of non-terminal name and its instance number
data LabelId = Return
                 String -- non-terminal
                 Int    -- instance number 
             | Alternate
                 String -- non-terminal
                 Int    -- alternate number
             | Finish
     deriving (Eq, Ord, Show)

data PState = PState
    { gstate :: GState L
    , input :: String
    , index :: Int
    , grammar :: LGrammar
    , result :: Bool
    }
type ParseM a = State PState a

moveToNextChar :: ParseM ()
moveToNextChar = do
    s <- get
    put $ s { index = index s + 1 }

getCurrentChar :: ParseM Char
getCurrentChar = do
    s <- get
    return $ input s !! index s

labelGrammar :: Grammar -> LGrammar
labelGrammar gr = fst $ foldl' f1 (M.empty, M.empty) gr
    where
    f1 (lgr,nonts) (nt, alts) = 
        let (lgr',nonts',_) = foldl' (f2 nt) (lgr,nonts,0) alts
        in (lgr',nonts')
    f2 nt (lgr,nonts,k) alt =
        let (labeled_alt,nonts') = foldr f3 ([], nonts) alt
        in (M.insertWith (++) nt [(k,labeled_alt)] lgr, nonts',k+1)
    f3 (Terminal c) (str,nonts) = (LTerminal c : str, nonts)
    f3 (Nonterminal nt) (str,nonts) =
        case M.lookup nt nonts of
            Nothing ->
                (LNonterminal 0 nt : str, M.insert nt 0 nonts)
            Just i ->
                (LNonterminal (i+1) nt : str, M.insert nt (i+1) nonts)

askGSS :: (GState L -> a) -> ParseM a
askGSS f = fmap f $ gets gstate

askAndModifyGSS :: (GState L -> (GState L,a)) -> ParseM a
askAndModifyGSS f = do
    s@PState{gstate = oldgs} <- get
    let (newgs,ret) = f oldgs
    put $ s { gstate = newgs }
    return ret

modifyGSS :: (GState L -> GState L) -> ParseM ()
modifyGSS f = do
    s@PState{gstate = oldgs} <- get
    let newgs = f oldgs
    put $ s { gstate = newgs }

pop :: ParseM ()
pop = gets index >>= modifyGSS . GSS.pop 

create :: LabelId -> ParseM Bool -> ParseM ()
create Alternate{} _ = error $ "Alternate cannot be used in GSS node"
create l c = gets index >>= modifyGSS . GSS.create (L l c)

add :: LabelId -> ParseM Bool -> ParseM()
add l c = gets index >>= modifyGSS . GSS.add (L l c)
    
l_0 = do
    mbDesc <- askAndModifyGSS GSS.fetchDescriptor
    case mbDesc of
        Nothing -> gets result
        Just desc@(l,i) -> do
            s <- get
            put $ s { index = i }
            goto l

finish = do
    s <- get
    when (index s == length (input s) - 1) $
        put $ s { result = True }
    l_0

parse_alternate :: Str -> ParseM Bool
parse_alternate [] = do pop; l_0
parse_alternate (sym:rest) = do
    case sym of
        LTerminal c -> do
            c' <- getCurrentChar
            if c == c'
                then do
                    moveToNextChar
                    parse_alternate rest
                else l_0
        LNonterminal i nt -> do
            create (Return nt i) (parse_alternate rest)
            parse_nt nt -- XXX we don't need this
                        -- if the node is already created

parse_nt :: String -> ParseM Bool
parse_nt nt = do
    gr <- gets grammar
    let alternates = gr M.! nt
    forM_ alternates $ \(i,alt) ->
        -- for every alternate add a descriptor
        add (Alternate nt i) $ parse_alternate alt
    l_0

parse :: Grammar -> String -> Bool
parse gr str = evalState go state
    where
    go = do
        create Finish finish
        parse_nt (fst.head$gr)
    state = PState { input = str ++ "\0"
                   , index = 0
                   , gstate = GSS.mkGState
                   , grammar = labelGrammar gr
                   , result = False
                   }
