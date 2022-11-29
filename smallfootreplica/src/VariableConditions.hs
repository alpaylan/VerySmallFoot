module VariableConditions
    (
        varC,
        varF,
        modC,
        modF,
        reqC,
        reqF,
        Context,
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

fva :: Assertion -> Set VarName
fva (AssertIfThenElse b a1 a2) = fv b \/ fv a1 \/ fv a2
fva (AssertConj bs hs) = Set.unions $ map fv bs \/ Set.unions $ map fv hs

fvhp :: HeapPredicate -> Set VarName
fvhp (PointsTo e h) = fv e \/ fv h
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
fvc (Block cs) = _
fvc (Call (_, xs, es)) = Set.fromList xs \/ fv es
fvc (ConcurrentCall call1 call2) =
  fvc (Call call2) \/ fvc (Call call2)
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


getFunction :: Context -> FunName -> Function
getFunction (_, delta) fname =
  fromMaybe (error "Function not found") $ Map.lookup fname delta

getResource :: Context -> ResName -> Resource
getResource (gamma, _) rname =
  fromMaybe (error "Resource not found") $ Map.lookup rname gamma


varC :: Context -> Command -> Set VarName
varC _ (Assign v expression) = Set.singleton v \/ fv expression
varC _ (HeapLookup v expression _) = Set.singleton v \/ fv expression
varC _ (HeapAssign e _ f) = fv e \/ fv f
varC _ (New x) = Set.singleton x
varC _ (Dispose e) = fv e
varC ctx (Block cs) = Set.unions $ map (varC ctx) cs
varC ctx (IfThenElse b c1 c2) = fv b \/ varC ctx c1 \/ varC ctx c2
varC ctx (While b i c) = fv b \/ fv i \/ varC ctx c
varC ctx (Call (f, ps, es)) =
  varF ctx fn \/ Set.fromList ps \/ fv es
  where fn = getFunction ctx f
varC ctx (ConcurrentCall call1 call2) =
  varC ctx (Call call1) \/ varC ctx (Call call2)
varC ctx (WithRes res b c) =
  ((fv b \/ varC ctx c) `Set.difference` fv inv) \/ (modC ctx c `Set.difference` owned ctx res)
  where Resource _ _ inv = getResource ctx res

varF :: Context -> Function -> Set VarName
varF ctx (Function _ refs vals locals (p, c, q)) =
  (varC ctx c \/ fv p \/ fv q) `Set.difference` Set.fromList (refs ++ vals ++ locals)





modC :: Context -> Command -> Set VarName
modC _ (Assign x _) = Set.singleton x
modC _ (HeapLookup x _ _) = Set.singleton x
modC _ (HeapAssign _ _ _) = Set.empty
modC _ (New x) = Set.singleton x
modC _ (Dispose _) = Set.empty
modC ctx (Block cs) = Set.unions $ map (modC ctx) cs
modC ctx (IfThenElse _ c1 c2) = modC ctx c1 \/ modC ctx c2
modC ctx (While _ _ c) = modC ctx c
modC ctx (Call (f, ps, _)) =
  modF ctx fn \/ Set.fromList ps
  where fn = getFunction ctx f
modC ctx (ConcurrentCall call1 call2) =
  modC ctx (Call call1) \/ modC ctx (Call call2)
modC ctx (WithRes r _ c) =
  modC ctx c `Set.difference` owned ctx r


modF :: Context -> Function -> Set VarName
modF ctx (Function _ refs vals locals (_, c, _)) =
    modC ctx c `Set.difference` Set.fromList (refs ++ vals ++ locals)


reqC :: Context -> Command -> Set ResName
reqC ctx (Block cs) = Set.unions $ map (reqC ctx) cs
reqC ctx (IfThenElse b c1 c2) = reqC ctx c1 \/ reqC ctx c2 \/ er ctx Set.empty (fv b)
reqC ctx (While b i c) = reqC ctx c \/ er ctx Set.empty (fv b \/ fv i)
reqC ctx (Call (f, xs, es)) =
  reqF ctx fn \/ er ctx (Set.fromList xs) (fv es)
  where fn = getFunction ctx f
reqC ctx (ConcurrentCall call1 call2) =
  reqC ctx (Call call1) \/ reqC ctx (Call call2)
reqC ctx (WithRes res b c) =
  Set.delete res (reqC ctx c \/ er ctx Set.empty (fv b))
reqC ctx s = er ctx (modC ctx s) (varC ctx s)

reqF :: Context -> Function -> Set ResName
reqF ctx (Function _ refs vals locals (p, c, q)) =
  reqC ctx c \/ er ctx Set.empty (fv p \/ fv q `Set.difference` Set.fromList (refs ++ vals ++ locals))


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
