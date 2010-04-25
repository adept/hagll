{-# OPTIONS_HADDOCK ignore-exports #-}
module Grammar (
  Symbol(..), Production, Grammar, parse
) where
import Control.Monad.State
import GSS (GState)
import qualified GSS
import qualified Data.Map as M
import Data.List

-- * User interface
-- | This is how user specifies the grammar using BNF -- as a list of
-- productions.
--
-- Empty (epsilon) productions are allowed. Single nonterminal may have multiple
-- productions associated with it.
type Grammar = [Production]

-- | Production
--
-- > a ::= a_1 | a_2 | ... | a_n
--
-- is encoded as a tuple
--
-- > (a, [a_1, a_2, ..., a_n])
--
-- where @a_i@ is a list of terminals and nonterminals.
type Production = (String, [[Symbol]])
data Symbol = Terminal Char | Nonterminal String
    deriving Show

-- | Recognizes whether the given input string is produced by the grammar
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

-- * Internal
-- ** Labeled grammar
-- | In order to form code labels we need grammar to have labeled alternates and
-- non-terminals, as described in ldta section 4.2 (p. 121)
--
-- We also use Map instead of association list in the internal representation of
-- the grammar.
type LGrammar = M.Map String [(Int,LStr)]
type LStr = [LSymbol]
data LSymbol = LTerminal Char
             | LNonterminal Int String
    deriving Show

-- | Transforms unlabeled grammar (as provided by the user) to a labeled one
-- using fold.
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

-- ** Code labels
-- | As described in ldta section 4.1 (p. 120) we have code labels of several
-- types:
--
-- * 'Return' labels are pushed to GSS and serve as a return address when we
-- pass control to 'parse_nt'. 'Return' label is identified by nonterminal (and
-- its instance number) for which the label is created.
--
-- * 'Finish' is the special kind of return label, which is return address for
-- the start symbol. Unlinke ldta, we do not use L_0 for this.
--
-- * 'Alternate' labels are used in 'parse_nt' to try to parse all alternates.
-- They are used only in descriptors and are never pushed to GSS.
--
-- * Unlike ldta we do not use \"Nonterminal labels\"; instead, we call the code
-- directly.
data LabelId = Return
                 String -- non-terminal
                 Int    -- instance number
             | Alternate
                 String -- non-terminal
                 Int    -- alternate number
             | Finish
     deriving (Eq, Ord, Show)

-- | Datatype for labels, which holds 'LabelId' and the code which should be
-- executed when we jump to this label.
--
-- This is a wrapper type for passing to GSS functions. Functions which work
-- with labels such as 'create' and 'add' take 'LabelId' and code as separate
-- arguments for convenience.
data L = L { ident :: LabelId, goto :: Code }
instance Eq L where
  (L id1 _) == (L id2 _) = id1 == id2
instance Ord L where
  compare (L id1 _) (L id2 _) = compare id1 id2
instance Show L where
  show (L id1 _) = show id1

-- ** Parse monad
data PState = PState
    { gstate :: GState L
    , input :: String
    , index :: Int
    , grammar :: LGrammar
    , result :: Bool
    }

type ParseM a = State PState a

type Code = ParseM Bool

moveToNextChar :: ParseM ()
moveToNextChar = do
    s <- get
    put $ s { index = index s + 1 }

getCurrentChar :: ParseM Char
getCurrentChar = do
    s <- get
    return $ input s !! index s

-- ** Generic GSS wrappers
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

-- ** GSS wrappers
pop :: ParseM ()
pop = gets index >>= modifyGSS . GSS.pop

create :: LabelId -> Code -> ParseM ()
create Alternate{} _ = error $ "Alternate cannot be used in GSS node"
create l c = gets index >>= modifyGSS . GSS.create (L l c)

add :: LabelId -> Code -> ParseM()
add l c = gets index >>= modifyGSS . GSS.add (L l c)

-- ** Parsing
-- | Fetchs a descriptor from GSS and executes it
l_0 :: Code
l_0 = do
    mbDesc <- askAndModifyGSS GSS.fetchDescriptor
    case mbDesc of
        Nothing -> gets result
        Just desc@(l,i) -> do
            s <- get
            put $ s { index = i }
            goto l

-- | 'finish' is called when start symbol is parsed successfully.
-- If the whole stream is consumed, record success in 'PState'
finish :: Code
finish = do
    s <- get
    when (index s == length (input s) - 1) $
        put $ s { result = True }
    l_0

-- | Tries to parse each symbol in sequence, calling 'parse_nt' for each
-- nonterminal.
parse_alternate :: LStr -> Code
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

-- | Tries to parse nonterminal by scheduling 'parse_alternate' for each
-- alternate.
parse_nt :: String -> Code
parse_nt nt = do
    gr <- gets grammar
    let alternates = gr M.! nt
    forM_ alternates $ \(i,alt) ->
        -- for every alternate add a descriptor
        add (Alternate nt i) $ parse_alternate alt
    l_0
