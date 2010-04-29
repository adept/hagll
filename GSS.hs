module GSS
 ( GState, Pos, create, add, pop, fetchDescriptor, mkGState )
where

import Data.Map as M
import Data.Set as S
import Data.Maybe
import Data.List

-- | Position in the input stream
type Pos = Int
-- | Type of GSS node. Nodes should be created using 'create'
data Node lab = Root | Node lab Pos deriving (Eq,Ord,Show) -- TODO: remove Show in production
type Descriptor lab = (lab, Node lab, Pos)
type R lab = [(lab, Node lab, Pos)]
type U lab = Set (lab,Node lab,Pos)
type P lab = Map (Node lab) [Pos]
type G lab = Set (Node lab)
type E lab = Map (Node lab) (Set (Node lab)) -- parents

-- | The state of GSS, whose nodes are labeled with labels of type @lab@
data GState lab = GState
    { gee :: G lab
    , er  :: R lab
    , pe  :: P lab
    , curr_u :: Node lab
    , parents :: E lab
    , yu  :: U lab
    }

-- | Creates initial 'GState'
mkGState :: GState lab
mkGState =
  GState { gee = S.singleton Root
         , parents = M.empty
         , curr_u = Root
         , pe = M.empty
         , er = []
         , yu = S.empty
         }

-- | Fetch descriptor from the /R/ set. Return 'Nothing' if /R/ is empty.
-- If /R/ is not empty, it is undefined what descriptor from the /R/ set will be
-- returned.
-- If a descriptor is returned, it is removed from /R/, and current node is
-- updated.
fetchDescriptor :: GState lab -> (GState lab, Maybe (lab, Pos))
fetchDescriptor gstate =
    case er gstate of
        [] -> (gstate, Nothing)
        d@(l,u,i):ds -> (gstate { er = ds, curr_u = u }, Just (l,i))

-- | @create l u i@ ensures that:
--
-- * node @v@ with label @l@ and input position @i@ exists in GSS
--
-- * node @v@ is a child of the current node
--
-- * if @v@ was already popped on position @i@, then descriptors are added to
-- /R/, as if @pop@ was executed after @create@
--
-- * node @v@ is made the current node
--
-- 'create' returns an updated GSS and an indicator whether new node has been
-- created (True) or the node already existed
create :: (Eq lab, Ord lab) => lab -> Pos -> GState lab
       -> (GState lab, Bool)
create label i oldgs =
    if node_exists && u `S.member` (parents oldgs M.! v)
    then -- nothing to do
        (oldgs, False)
    else
        (set_current.add_popped.connect_v.insert_v $ oldgs, not node_exists)
    where
    node_exists = v `S.member` g
    g = gee oldgs
    p = pe oldgs
    v = Node label i
    u = curr_u oldgs
    insert_v gstate = gstate { gee = S.insert v g }
    connect_v gstate = gstate { parents = M.insertWith (S.union) v (S.singleton u) (parents gstate) }
    add_popped gstate = foldl' (\gs j -> add1 (label, u, j) gs) gstate popped
    popped = fromMaybe [] (M.lookup v p)
    set_current gstate = gstate { curr_u = v }

-- | Adds descriptor to /R/ if it hasn't been added yet
--
-- For internal use only! Outside the module use 'add'
add1 :: (Eq lab, Ord lab) => Descriptor lab -> GState lab -> GState lab
add1 desc oldgs =
    if not (desc `S.member` yu_)
        then oldgs {er = desc:r, yu = S.insert desc yu_}
        else oldgs
    where
    r = er oldgs
    yu_ = yu oldgs

-- | Adds descriptor to /R/ if it hasn't been added yet
add :: (Eq lab, Ord lab) => lab -> Pos -> GState lab -> GState lab
add l i oldgs =
    if not (desc `S.member` yu_)
        then oldgs {er = desc:r, yu = S.insert desc yu_}
        else oldgs
    where
    desc = (l, curr_u oldgs, i)
    r = er oldgs
    yu_ = yu oldgs

-- | @pop i@ adds descriptors (@label(u)@, @v@, @i@) to /R/ for every parent @v@ of
-- @u@, where @u@ is the current node.
pop :: (Eq lab, Ord lab) => Pos -> GState lab -> GState lab
pop i oldgs = if u == Root then oldgs else newgs
    where
    u = curr_u oldgs
    Node label _ = u
    prnts = parents oldgs
    update_pe gstate = gstate { pe = M.insertWith (++) u [i] (pe gstate) }
    create_descriptors gstate = foldl (\gs parent -> add1 (label, parent, i) gs) gstate (S.elems (prnts!u))
    newgs = create_descriptors . update_pe $ oldgs
