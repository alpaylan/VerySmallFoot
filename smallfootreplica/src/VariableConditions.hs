module VariableConditions
    (
        varC,
        varF,
        modC,
        modF,
        reqC,
        reqF,
        par,
        Context,
        mkContext,
        getFunction,
        getResource,
        FV,
        fv
    )
    where

import Program

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Data.Maybe

(\/) :: (Ord a) => Set a -> Set a -> Set a
(\/) = Set.union

(/\) :: (Ord a) => Set a-> Set a-> Set a
(/\) = Set.intersection


fve :: Expression -> Set VarName
fve (Var v) = Set.singleton v
fve (Const _) = Set.empty
fve (Xor e1 e2) = fv e1 \/ fv e2
fve Nil = Set.empty

fvb :: BoolExpression -> Set VarName
fvb (BoolEq e1 e2) = fv e1 \/ fv e2
fvb (BoolNEq e1 e2) = fv e1 \/ fv e2
fvb BoolTrue = Set.empty
fvb BoolFalse = Set.empty

fva :: Assertion -> Set VarName
fva (AssertIfThenElse b a1 a2) = fv b \/ fv a1 \/ fv a2
fva (AssertConj bs hs) = Set.unions (map fv bs) \/ Set.unions (map fv hs)

fvhp :: HeapPredicate -> Set VarName
fvhp (PointsTo e h) = fv e \/ Set.unions (map (fv . snd) h)
fvhp (HeapTree e) = fv e
fvhp (HeapLinkedList e1 e2) = fv e1 \/ fv e2
fvhp (HeapXORList e1 e2 e3 e4) = fv e1 \/ fv e2 \/ fv e3 \/ fv e4

fvc :: Command -> Set VarName
fvc (Assign x e) = Set.singleton x \/ fv e
fvc (HeapLookup x e _) = Set.singleton x \/ fv e
fvc (HeapAssign e1 _ e2) = fv e1 \/ fv e2
fvc (New x) = Set.singleton x
fvc (Dispose e) = fv e
fvc (IfThenElse b c1 c2) = fv b \/ fv c1 \/ fv c2
fvc (While b i c) = fv b \/ fv i \/ fv c
fvc (Block cs) = Set.unions $ map fvc cs
fvc (Call (_, xs, es)) = Set.fromList xs \/ fv es
fvc (ConcurrentCall call1 call2) =
  fvc (Call call1) \/ fvc (Call call2)
fvc (WithRes _ b c) =
  fvb b \/ fvc c


class FV a where
    fv :: a -> Set VarName
instance FV Expression where
    fv = fve
instance FV BoolExpression where
    fv = fvb
instance FV Assertion where
    fv = fva
instance FV HeapPredicate where
    fv = fvhp
instance FV a => FV [a] where
    fv = Set.unions . map fv
instance FV Command where
    fv = fvc


-- (Gamma, Delta)
type Context = (Map ResName Resource, Map FunName Function)


resName :: Resource -> ResName
resName (Resource name _ _) = name

funName :: Function -> FunName
funName (Function name _ _ _ _) = name


mkContext :: Program -> Context
mkContext (Program _ rs fs) =
    (Map.fromList $ map (\r -> (resName r, r)) rs,
     Map.fromList $ map (\f -> (funName f, f)) fs)


getFunction :: Context -> FunName -> Function
getFunction (_, delta) fname =
  fromMaybe (error "Function not found") $ Map.lookup fname delta

getResource :: Context -> ResName -> Resource
getResource (gamma, _) rname =
  fromMaybe (error "Resource not found") $ Map.lookup rname gamma


getCalls :: Context -> FunName -> Set FunName
getCalls ctx fname = getCallsF (getFunction ctx fname)
  where
    getCallsF :: Function -> Set FunName
    getCallsF (Function _ _ _ _ (_, c, _)) = getCallsC c

    getCallsC :: Command -> Set FunName
    getCallsC (Assign _ _) = Set.empty
    getCallsC (HeapLookup {}) = Set.empty
    getCallsC (HeapAssign {}) = Set.empty
    getCallsC (New _) = Set.empty
    getCallsC (Dispose _) = Set.empty
    getCallsC (IfThenElse _ c1 c2) = getCallsC c1 \/ getCallsC c2
    getCallsC (While _ _ c) = getCallsC c
    getCallsC (Call (f, _, _)) = Set.singleton f
    getCallsC (ConcurrentCall (f1, _, _) (f2, _, _)) = Set.fromList [f1, f2]
    getCallsC (WithRes _ _ c) = getCallsC c
    getCallsC (Block cs) = Set.unions $ map getCallsC cs


-- todo: make this efficient.
getCalledFunctions :: Context -> FunName -> Set FunName
getCalledFunctions ctx fname =
  go ctx (Set.singleton fname) fname 
  where
    go :: Context -> Set FunName -> FunName -> Set FunName
    go ctx visited fname  =
      let
        calls = getCalls ctx fname
        unvisitedCalls = calls `Set.difference` visited
        visited' = visited \/ unvisitedCalls
      in
        if Set.null unvisitedCalls
        then visited
        else Set.unions $ Set.map (go ctx visited') unvisitedCalls




genericCalled :: (Context -> Function -> Set String) -> Context -> FunName -> Set String
genericCalled f ctx fname =
  let functions = getCalledFunctions ctx fname in
  Set.unions $ map (f ctx . getFunction ctx) (Set.toList functions)


varCalled :: Context -> FunName -> Set VarName
varCalled = genericCalled varFNonRec

modCalled :: Context -> FunName -> Set VarName
modCalled = genericCalled modFNonRec

reqCalled :: Context -> FunName -> Set ResName
reqCalled = genericCalled reqFNonRec





varC :: Context -> Command -> Set VarName
varC _ (Assign v expression) = Set.singleton v \/ fv expression
varC _ (HeapLookup v expression _) = Set.singleton v \/ fv expression
varC _ (HeapAssign e _ f) = fv e \/ fv f
varC _ (New x) = Set.singleton x
varC _ (Dispose e) = fv e
varC ctx (Block cs) = Set.unions $ map (varC ctx) cs
varC ctx (IfThenElse b c1 c2) = fv b \/ varC ctx c1 \/ varC ctx c2
varC ctx (While b i c) = fv b \/ fv i \/ varC ctx c
varC _ (Call (_, ps, es)) = Set.fromList ps \/ fv es
varC ctx (ConcurrentCall call1 call2) =
  varC ctx (Call call1) \/ varC ctx (Call call2)
varC ctx (WithRes res b c) =
  ((fv b \/ varC ctx c) `Set.difference` fv inv) \/ (modC ctx c `Set.difference` owned ctx res)
  where Resource _ _ inv = getResource ctx res


varFNonRec :: Context -> Function -> Set VarName
varFNonRec ctx (Function fname refs vals locals (p, c, q)) =
  (varC ctx c \/ fv p \/ fv q) `Set.difference` Set.fromList (refs ++ vals ++ locals)

varF :: Context -> Function -> Set VarName
varF ctx (Function fname refs vals locals (p, c, q)) =
  (varCalled ctx fname \/ varC ctx c \/ fv p \/ fv q) `Set.difference` Set.fromList (refs ++ vals ++ locals)





modC :: Context -> Command -> Set VarName
modC _ (Assign x _) = Set.singleton x
modC _ (HeapLookup x _ _) = Set.singleton x
modC _ (HeapAssign _ _ _) = Set.empty
modC _ (New x) = Set.singleton x
modC _ (Dispose _) = Set.empty
modC ctx (Block cs) = Set.unions $ map (modC ctx) cs
modC ctx (IfThenElse _ c1 c2) = modC ctx c1 \/ modC ctx c2
modC ctx (While _ _ c) = modC ctx c
modC _ (Call (_, ps, _)) = Set.fromList ps
modC ctx (ConcurrentCall call1 call2) =
  modC ctx (Call call1) \/ modC ctx (Call call2)
modC ctx (WithRes r _ c) =
  modC ctx c `Set.difference` owned ctx r


modFNonRec :: Context -> Function -> Set VarName
modFNonRec ctx (Function fname refs vals locals (_, c, _)) =
    modC ctx c `Set.difference` Set.fromList (refs ++ vals ++ locals)

modF :: Context -> Function -> Set VarName
modF ctx (Function fname refs vals locals (_, c, _)) =
    (modC ctx c \/ modCalled ctx fname) `Set.difference` Set.fromList (refs ++ vals ++ locals)


reqC :: Context -> Command -> Set ResName
reqC ctx (Block cs) = Set.unions $ map (reqC ctx) cs
reqC ctx (IfThenElse b c1 c2) = reqC ctx c1 \/ reqC ctx c2 \/ er ctx Set.empty (fv b)
reqC ctx (While b i c) = reqC ctx c \/ er ctx Set.empty (fv b \/ fv i)
reqC ctx (Call (_, xs, es)) =
  er ctx (Set.fromList xs) (fv es)
reqC ctx (ConcurrentCall call1 call2) =
  reqC ctx (Call call1) \/ reqC ctx (Call call2)
reqC ctx (WithRes res b c) =
  Set.delete res (reqC ctx c \/ er ctx Set.empty (fv b))
reqC ctx s = er ctx (modC ctx s) (varC ctx s)


reqFNonRec :: Context -> Function -> Set ResName
reqFNonRec ctx (Function fname refs vals locals (p, c, q)) =
  reqC ctx c \/ er ctx Set.empty (fv p \/ fv q `Set.difference` Set.fromList (refs ++ vals ++ locals))

reqF :: Context -> Function -> Set ResName
reqF ctx (Function fname refs vals locals (p, c, q)) =
  reqC ctx c \/ reqCalled ctx fname  \/ er ctx Set.empty (fv p \/ fv q `Set.difference` Set.fromList (refs ++ vals ++ locals))


owned :: Context -> ResName -> Set VarName
owned ctx resourceName =
  Set.fromList resources
  where (Resource _ resources _) = getResource ctx resourceName

var :: Context -> ResName -> Set VarName
var ctx r =
  owned ctx r \/ fv inv
  where (Resource _ _ inv) = getResource ctx r


type ModifiedVariables = Set VarName
type AccessedVariables = Set VarName
er :: Context -> ModifiedVariables -> AccessedVariables -> Set ResName
er (gamma, _) m a =
  Set.fromList
    [r | (_, Resource r xs inv) <- Map.toList gamma,
         not (null (a /\ Set.fromList xs))
         ||
         not (null (m /\ Set.fromList xs \/ fv inv))]



getConcurrentCalls :: Function -> Set (FunName, FunName)
getConcurrentCalls (Function _ _ _ _ (_, c, _)) =
  getConcurrentCallsC c
  where
    getConcurrentCallsC :: Command -> Set (FunName, FunName)
    getConcurrentCallsC (ConcurrentCall (f1, _, _) (f2, _, _)) =
      Set.singleton (f1, f2)
    getConcurrentCallsC (Block cs) =
      Set.unions $ map getConcurrentCallsC cs
    getConcurrentCallsC (IfThenElse _ c1 c2) =
      getConcurrentCallsC c1 \/ getConcurrentCallsC c2
    getConcurrentCallsC (While _ _ c) =
      getConcurrentCallsC c
    getConcurrentCallsC (WithRes _ _ c) =
      getConcurrentCallsC c
    getConcurrentCallsC _ = Set.empty

  
par :: Context -> FunName -> Set FunName
par ctx fname =
  let functions = Map.elems $ snd ctx in
  let allPairs = Set.unions $ Set.fromList (map getConcurrentCalls functions) in
  let relevantPairs = Set.filter (\(f1, f2) -> f1 == fname || f2 == fname) allPairs in
  let coCalledFunctions = Set.map (\(f1, f2) -> if f1 == fname then f2 else f1) relevantPairs in
  Set.unions $ Set.map (getCalledFunctions ctx) coCalledFunctions