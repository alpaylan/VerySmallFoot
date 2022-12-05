module VCGen (
  generateSymbolicProgram, ppSymbolicProgram,
  fvs, fresh, FreshVars, SymbolicHoareTriple, SCommand(..)
  ) where

import Control.Monad.State
    ( MonadState(put, get), evalState, State )

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Data.Foldable
import Data.Maybe

import Text.PrettyPrint

import Program

(\/) :: (Ord a) => Set a -> Set a -> Set a
(\/) = Set.union

(/\) :: (Ord a) => Set a -> Set a -> Set a
(/\) = Set.intersection

type Gamma = [Resource]
type Delta = [Function]

lookupGamma :: ResName -> Gamma -> Resource
lookupGamma r = fromJust . find (\(Resource r' _ _) -> r == r')

lookupDelta :: FunName -> Delta -> Function
lookupDelta f = fromJust . find (\(Function f' _ _ _ _) -> f == f')

data ModVarReq = MVR
  { md :: Set VarName
  , vr :: Set VarName
  , rq :: Set VarName
  }
  deriving (Eq)

emptyMVR :: ModVarReq
emptyMVR = MVR {md=Set.empty, vr=Set.empty, rq=Set.empty}

mvrUnion :: ModVarReq -> ModVarReq -> ModVarReq
mvrUnion mvr1 mvr2 = MVR {md=md mvr1 \/ md mvr2, vr=vr mvr1 \/ vr mvr2, rq=rq mvr1 \/ rq mvr2}

mvrUnions :: [ModVarReq] -> ModVarReq
mvrUnions = foldl' mvrUnion emptyMVR

mvrBody :: Gamma -> Map FunName ModVarReq -> Command -> ModVarReq
mvrBody gamma fmvr = f
  where
    f :: Command -> ModVarReq
    f (Assign x e) =
      MVR {md=md', vr=vr', rq=er' md' vr'}
      where md' = Set.singleton x
            vr' = md' \/ fv e
    f (HeapLookup x e _) =
      MVR {md=md', vr=vr', rq=er' md' vr'}
      where md' = Set.singleton x
            vr' = md' \/ fv e
    f (HeapAssign e _ fld) =
      MVR {md=md', vr=vr', rq=er' md' vr'}
      where md' = Set.empty
            vr' = fv e \/ fv fld
    f (New x) =
      MVR {md=md', vr=vr', rq=er' md' vr'}
      where md' = Set.singleton x
            vr' = md'
    f (Dispose e) =
      MVR {md=md', vr=vr', rq=er' md' vr'}
      where md' = Set.empty
            vr' = fv e
    f (Block cs) = mvrUnions (map f cs)
    f (IfThenElse b c1 c2) =
      MVR {md=md', vr=vr', rq=er' md' vr'} `mvrUnion` f c1 `mvrUnion` f c2
      where md' = Set.empty
            vr' = fv b
    f (While b i c) =
      MVR {md=md', vr=vr', rq=er' md' vr'} `mvrUnion` f c
      where md' = Set.empty
            vr' = fv b \/ fv i
    f (Call (g, xs, es)) =
      MVR {md=md', vr=vr', rq=er' md' vr'} `mvrUnion` (fmvr Map.! g)
      where md' = Set.fromList xs
            vr' = md' \/ fv es
    f (ConcurrentCall call1 call2) = f (Call call1) `mvrUnion` f (Call call2)
    f (WithRes r b c) =
      MVR {md=md', vr=md' \/ ((fvb \/ vr cmvr) `Set.difference` fv inv),
           rq=Set.delete r (rq cmvr \/ er' Set.empty fvb)}
      where Resource _ xs inv = lookupGamma r gamma
            cmvr = f c
            md' = md cmvr `Set.difference` Set.fromList xs
            fvb = fv b

    er' = er gamma

er :: Gamma -> Set VarName -> Set VarName -> Set VarName
er gamma m a =
  Set.fromList
    [r | Resource r xs inv <- gamma,
         not (null (a /\ Set.fromList xs))
         ||
         not (null (m /\ Set.fromList xs \/ fv inv))]


lfp :: (Eq a) => a -> (a -> a) -> a
lfp a f
  | a' == a = a
  | otherwise = lfp a' f
  where a' = f a

calcCallGraph :: [Function] -> Map FunName (Map FunName (Set ResName))
calcCallGraph fs = lfp initialGraph propCalls
  where
    propCalls m = Map.map (propCallsF m) m
    propCallsF m s = foldl' (\acc f -> Map.unionWith (\/) acc (m Map.! f)) s (Map.keys s)

    initialGraph = Map.fromList $ map (\f@(Function fn _ _ _ _) -> (fn, initialCalls f)) fs
    initialCalls (Function _ _ _ _ (_, c, _)) = callsC Set.empty c

    callsC :: Set ResName -> Command -> Map FunName (Set ResName)
    callsC _ (Assign _ _) = Map.empty
    callsC _ (HeapLookup {}) = Map.empty
    callsC _ (HeapAssign {}) = Map.empty
    callsC _ (New _) = Map.empty
    callsC _ (Dispose _) = Map.empty
    callsC acq (Block cs) = Map.unionsWith (\/) $ map (callsC acq) cs
    callsC acq (IfThenElse _ c1 c2) = Map.unionWith (\/) (callsC acq c1) (callsC acq c2)
    callsC acq (While _ _ c) = callsC acq c
    callsC acq (Call (f, _, _)) = Map.singleton f acq
    callsC acq (ConcurrentCall (f1, _, _) (f2, _, _)) = Map.fromList [(f1, acq), (f2, acq)]
    callsC acq (WithRes r _ c) = callsC (Set.insert r acq) c


calcPars :: Delta -> Map FunName (Map FunName (Set ResName)) -> Map FunName (Set FunName)
calcPars delta calls = Map.unionsWith Set.union $ map parF delta
  where
    parF :: Function -> Map FunName (Set FunName)
    parF (Function _ _ _ _ (_, c, _)) = parC c

    parC :: Command -> Map FunName (Set FunName)
    parC (IfThenElse _ c1 c2) = Map.unionWith Set.union (parC c1) (parC c2)
    parC (While _ _ c) = parC c
    parC (ConcurrentCall (f1, _, _) (f2, _, _)) =
      Map.fromList [ (f1, Set.insert f2 $ Map.keysSet (calls Map.! f2))
                   , (f2, Set.insert f1 $ Map.keysSet (calls Map.! f1))
                   ]
    parC (WithRes _ _ c) = parC c
    parC (Block cs) = Map.unionsWith Set.union (map parC cs)
    parC _ = Map.empty


data FunInfo = FunInfo
  { fiMVR :: ModVarReq
  , fiCalls :: Map FunName (Set ResName)
  , fiPars :: Set FunName
  }

calcFunInfo :: Gamma -> Delta -> Map FunName FunInfo
calcFunInfo gamma delta = Map.fromList $ map funInfo delta
  where
    funInfo (Function f _ _ _ _) =
      ( f
      , FunInfo
          { fiMVR = mvrDeep Map.! f
          , fiCalls = calls Map.! f
          , fiPars = pars Map.! f })

    calls = calcCallGraph delta
    pars = calcPars delta calls

    emptyMVRs = Map.fromList $ map (\(Function f _ _ _ _) -> (f, emptyMVR)) delta

    mvrF (Function _ refs vals locals (p, c, q)) =
      MVR { md=md mvr `Set.difference` bound
          , vr=(vr mvr \/ fvPQ) `Set.difference` bound
          , rq=rq mvr \/ er gamma Set.empty (fvPQ `Set.difference` bound)}
      where
        fvPQ = fv p \/ fv q
        bound = Set.fromList $ refs ++ vals ++ locals
        mvr = mvrBody gamma emptyMVRs c

    mvrShallow = Map.fromList $ map (\f@(Function n _ _ _ _) -> (n, mvrF f)) delta

    mvrDeep = lfp mvrShallow propMVR

    propMVR :: Map FunName ModVarReq -> Map FunName ModVarReq
    propMVR m = Map.mapWithKey (propMVRF m) m
    propMVRF m f mvr = Map.foldlWithKey' addF mvr (calls Map.! f)
      where
        addF mvr' g acq =
          MVR { md=md mvr' \/ (md mvr'' `Set.difference` own)
              , vr=vr mvr' \/ (vr mvr'' `Set.difference` own)
              , rq=rq mvr' \/ (rq mvr'' `Set.difference` acq) }
          where mvr'' = m Map.! g
                own = owned acq

    owned :: Set ResName -> Set VarName
    owned = Set.unions . Set.map (\r -> resOwned $ lookupGamma r gamma)

    resOwned (Resource _ xs _) = Set.fromList xs

data SCommand
    = SAssign VarName Expression
    | SHeapLookup VarName Expression FieldName
    | SHeapAssign Expression FieldName Expression
    | SNew VarName
    | SDispose Expression
    | SBlock [SCommand]
    | SJump Precondition (Set VarName) Postcondition
    | SIfThenElse BoolExpression SCommand SCommand
    deriving Show

ppSCommand :: SCommand -> Doc
ppSCommand (SAssign x e) = text x <+> text ":=" <+> ppExpression e
ppSCommand (SHeapLookup x e f) = text x <+> text ":=" <+> ppExpression e <+> text "->" <+> text f
ppSCommand (SHeapAssign e1 f e2) = ppExpression e1 <+> text "->" <+> text f <+> text ":=" <+> ppExpression e2
ppSCommand (SNew x) = text "new" <+> text x <+> text "()"
ppSCommand (SDispose e) = text "dispose" <+> ppExpression e
ppSCommand (SBlock cs) = text "{" $$ nest 2 (vcat $ map ppSCommand cs) $$ text "}"
ppSCommand (SJump pre xs post) = text "jsr" <+> ppProp pre <+> hsep (map text $ Set.toList xs) <+> ppProp post
ppSCommand (SIfThenElse b c1 c2) = text "if" <+> ppBoolExpression b $$ nest 2 (ppSCommand c1) $$ text "else" $$ nest 2 (ppSCommand c2)


type SymbolicHoareTriple = (Precondition, SCommand, Postcondition)

ppSymbolicHoareTriple :: SymbolicHoareTriple -> Doc
ppSymbolicHoareTriple (pre, c, post) = ppProp pre $$ nest 2 (ppSCommand c) $$ ppProp post

ppSymbolicProgram :: [[SymbolicHoareTriple]] -> Doc
ppSymbolicProgram = vcat . map (vcat . map ppSymbolicHoareTriple)

type FreshVars = State [VarName]


fvs :: String -> [VarName]
fvs pre =
  fvs' 0
  where
    fvs' :: Integer -> [VarName]
    fvs' n = (pre ++ show n) : fvs' (n + 1)


fresh :: FreshVars VarName
fresh = do
  xs <- get
  put $ tail xs
  return $ head xs

type Context = (Gamma, Delta, Map FunName FunInfo)

ctxGamma :: Context -> Gamma
ctxGamma (gamma, _, _) = gamma

ctxDelta :: Context -> Delta
ctxDelta (_, delta, _) = delta

ctxFunInfo :: Context -> FunName -> FunInfo
ctxFunInfo (_, _, fi) = (fi Map.!)

vcg' :: Context -> FunName -> HoareTriple -> FreshVars [SymbolicHoareTriple]
vcg' ctx g (p, c, q) = do
  (si, l) <- chop' ctx g c
  return $ (p, si, q) : l


varAsserts :: [VarName] -> [Expression] -> PureProp
varAsserts vs es =
  foldr (\(v, e) acc -> PropAnd (PropAssert (BoolEq (Var v) e)) acc)
        PropTrue
        (zip vs es)

mod' :: Context -> Command -> Set VarName
mod' (gamma, _, fi) = md . mvrBody gamma (Map.map fiMVR fi)

chop' :: Context -> FunName -> Command -> FreshVars (SCommand, [SymbolicHoareTriple])
chop' _ _ (Assign x e) = return (SAssign x e, [])
chop' _ _ (HeapLookup x e f) = return (SHeapLookup x e f, [])
chop' _ _ (HeapAssign e1 f e2) = return (SHeapAssign e1 f e2, [])
chop' _ _ (New x) = return (SNew x, [])
chop' _ _ (Dispose e) = return (SDispose e, [])
chop' ctx g (Block cs) = do
  res <- mapM (chop' ctx g) cs
  return (SBlock $ map fst res, concatMap snd res)
chop' ctx g (IfThenElse b c1 c2) = do
  (si1, l1) <- chop' ctx g c1
  (si2, l2) <- chop' ctx g c2
  return (SIfThenElse b si1 si2, l1 ++ l2)
chop' ctx g (While b i c) = do
  l <- vcg' ctx g (extendPropAnd i b, c, i)
  return (SJump
          i
          (mod' ctx c)
          (extendPropAnd i (BoolNot b)),
          l)
chop' ctx _ (Call (f, xs, es)) = do
  v' <- mapM (const fresh) es
  let m = zip p xs ++ zip v v'
  return
    (SBlock
       [ SJump (PropConj PropTrue HeapEmp)
               Set.empty
               (PropConj (varAsserts v' es) HeapEmp)
       , SJump (subst m fp)
               (Set.map (substVar m) (mod' ctx fc))
               (subst m fq)
       ],
     [])
  where
    Function _ p v _ (fp, fc, fq) = lookupDelta f $ ctxDelta ctx
chop' ctx _ (ConcurrentCall (f1, xs1, es1) (f2, xs2, es2)) = do
  v1' <- mapM (const fresh) es1
  v2' <- mapM (const fresh) es2
  let m1 = zip p1 xs1 ++ zip v1 v1'
      m2 = zip p2 xs2 ++ zip v2 v2'
  return
    (SBlock
       [ SJump (PropConj PropTrue HeapEmp)
               Set.empty
               (PropConj (PropAnd (varAsserts v1' es1) (varAsserts v2' es2))
                         HeapEmp)
       , SJump (propSepConj (subst m1 fp1) (subst m2 fp2))
               (Set.map (substVar m1) (mod' ctx fc1) \/ Set.map (substVar m2) (mod' ctx fc2))
               (propSepConj (subst m1 fq1) (subst m2 fq2))
       ],
     [])
  where
    Function _ p1 v1 _ (fp1, fc1, fq1) = lookupDelta f1 $ ctxDelta ctx
    Function _ p2 v2 _ (fp2, fc2, fq2) = lookupDelta f2 $ ctxDelta ctx
chop' ctx g (WithRes res b c) = do
  (si, l) <- chop' ctx g c
  return
    (SBlock
       [ SJump (PropConj PropTrue HeapEmp)
               Set.empty
               (extendPropAnd r b)
       , si
       , SJump r
               (Set.fromList xs \/ u)
               (PropConj PropTrue HeapEmp)
       ],
     l)
  where
    Resource _ xs r = lookupGamma res $ ctxGamma ctx
    u = fv r /\ Set.unions (Set.map (md . fiMVR . ctxFunInfo ctx)
                                    (fiPars $ ctxFunInfo ctx g))


chop :: Context -> FunName -> Command -> (SCommand, [SymbolicHoareTriple])
chop ctx g c = evalState (chop' ctx g c) (fvs "_fv")


vcg :: Context -> Function -> [SymbolicHoareTriple]
vcg ctx g =
  let Function f _ _ _ (fp, fc, fq) = g in
  let (si, l) = chop ctx f fc in
  (fp, si, fq) : l


generateSymbolicProgram :: Program -> [[SymbolicHoareTriple]]
generateSymbolicProgram (Program _ gamma delta) =
  let ctx = (gamma, delta, calcFunInfo gamma delta) in
  map (vcg ctx) delta
