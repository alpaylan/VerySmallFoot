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

(\/) :: Set VariableName -> Set VariableName -> Set VariableName
(\/) = Set.union

(/\) :: Set VariableName -> Set VariableName -> Set VariableName
(/\) = Set.intersection


fve :: Expression -> Set VariableName
fve (Variable v) = Set.singleton v
fve (Constant _) = Set.empty
fve (Xor e1 e2) = fv e1 \/ fv e2
fve Nil = Set.empty

fvb :: BoolExpression -> Set VariableName
fvb (BoolEquals e1 e2) = fv e1 \/ fv e2
fvb (BoolNotEquals e1 e2) = fv e1 \/ fv e2

fvbp :: BooleanPredicate -> Set VariableName
fvbp (Conjunction exprs) = Set.unions $ map fv exprs
fvbp _ = Set.empty

fva :: Assertion -> Set VariableName
fva (SingleBooleanPredicate b) = fv b
fva (SingleHeapPredicate h) = fv h
fva (AssertionConjunction booleanPredicate heapPredicate) =
  fv booleanPredicate \/ fv heapPredicate
fva (AssertionIfThenElse b a1 a2) = fv b \/ fv a1 \/ fv a2

fvhp :: HeapPredicate -> Set VariableName
fvhp Emp = Set.empty
fvhp (SinglePredicate hp) = fv hp
fvhp (SeparatingConjunction heaplets) = Set.unions $ map fv heaplets

fvhlp :: HeapletPredicate -> Set VariableName
fvhlp (PointsToHeap e h) = fv e \/ fv h
fvhlp (HeapTree e) = fv e
fvhlp (HeapLinkedList e1 e2) = fv e1 \/ fv e2
fvhlp (HeapXORList e1 e2 e3 e4) = fv e1 \/ fv e2 \/ fv e3 \/ fv e4

fvp :: Heap -> Set VariableName
fvp (Heap h) = Set.unions $ map fv h

fvhr :: HeapRecord -> Set VariableName
fvhr (HeapRecord _ e) = fv e

fvas :: Assignment -> Set VariableName
fvas (VariableAssignment x e) = Set.singleton x \/ fv e
fvas (HeapLookup x e _) = Set.singleton x \/ fv e
fvas (HeapMutation e1 _ e2) = fv e1 \/ fv e2
fvas (Allocation x) = Set.singleton x
fvas (Deallocation e) = fv e

fvc :: Command -> Set VariableName
fvc (Assignment assignment) = fv assignment
fvc (IfThenElse b c1 c2) = fv b \/ fv c1 \/ fv c2
fvc (WhileDo b i c) = fv b \/ fv i \/ fv c
fvc (Sequence c1 c2) = fv c1 \/ fv c2
fvc (Call _ xs es) = Set.fromList xs \/ fv es
fvc (ConcurrentCall f1 p1 v1 f2 p2 v2) =
  fvc (Call f1 p1 v1) \/ fvc (Call f2 p2 v2)
fvc (WithResourceWhen _ b c) =
  fvb b \/ fvc c




class FV a where
    fv :: a -> Set VariableName
instance FV Expression where
    fv = fve
instance FV BoolExpression where
    fv = fvb
instance FV BooleanPredicate where
    fv = fvbp
instance FV Assertion where
    fv = fva
instance FV HeapPredicate where
    fv = fvhp
instance FV HeapletPredicate where
    fv = fvhlp
instance FV Heap where
    fv = fvp
instance FV HeapRecord where
    fv = fvhr
instance FV a => FV [a] where
    fv = Set.unions . map fv
instance FV Command where
    fv = fvc
instance FV Assignment where
    fv = fvas



-- (Gamma, Delta)
type Context = (Map ResourceName Resource, Map FunctionName Function)


getFunction :: Context -> FunctionName -> Function
getFunction (_, delta) fname =
  fromMaybe (error "Function not found") $ Map.lookup fname delta

getResource :: Context -> ResourceName -> Resource
getResource (gamma, _) rname =
  fromMaybe (error "Resource not found") $ Map.lookup rname gamma


varC :: Context -> Command -> Set VariableName
varC _ (Assignment (VariableAssignment v expression)) = Set.singleton v \/ fv expression
varC _ (Assignment (HeapLookup v expression _)) = Set.singleton v \/ fv expression
varC _ (Assignment (HeapMutation e _ f)) = fv e \/ fv f
varC _ (Assignment (Allocation x)) = Set.singleton x
varC _ (Assignment (Deallocation e)) = fv e
varC ctx (Sequence c1 c2) = varC ctx c1 \/ varC ctx c2
varC ctx (IfThenElse b c1 c2) = fv b \/ varC ctx c1 \/ varC ctx c2
varC ctx (WhileDo b i c) = fv b \/ fv i \/ varC ctx c
varC ctx (Call f ps es) =
  varF ctx fn \/ Set.fromList ps \/ fv es
  where fn = getFunction ctx f
varC ctx (ConcurrentCall f1 r1 v1 f2 r2 v2) =
  varC ctx (Call f1 r1 v1) \/ varC ctx (Call f2 r2 v2)
varC ctx (WithResourceWhen res b c) =
  ((fv b \/ varC ctx c) `Set.difference` fv inv) \/ (modC ctx c `Set.difference` owned ctx res)
  where Resource _ _ inv = getResource ctx res

varF :: Context -> Function -> Set VariableName
varF ctx (Function _ refs vals locals (p, c, q)) =
  (varC ctx c \/ fv p \/ fv q) `Set.difference` Set.fromList (refs ++ vals ++ locals)





modC :: Context -> Command -> Set VariableName
modC _ (Assignment (VariableAssignment x _)) = Set.singleton x
modC _ (Assignment (HeapLookup x _ _)) = Set.singleton x
modC _ (Assignment (HeapMutation _ _ _)) = Set.empty
modC _ (Assignment (Allocation x)) = Set.singleton x
modC _ (Assignment (Deallocation _)) = Set.empty
modC ctx (Sequence c1 c2) = modC ctx c1 \/ modC ctx c2
modC ctx (IfThenElse _ c1 c2) = modC ctx c1 \/ modC ctx c2
modC ctx (WhileDo _ _ c) = modC ctx c
modC ctx (Call f ps _) =
  modF ctx fn \/ Set.fromList ps
  where fn = getFunction ctx f
modC ctx (ConcurrentCall f1 r1 v1 f2 r2 v2 ) =
  modC ctx (Call f1 r1 v1) \/ modC ctx (Call f2 r2 v2)
modC ctx (WithResourceWhen r _ c) =
  modC ctx c `Set.difference` owned ctx r


modF :: Context -> Function -> Set VariableName
modF ctx (Function _ refs vals locals (_, c, _)) =
    modC ctx c `Set.difference` Set.fromList (refs ++ vals ++ locals)


reqC :: Context -> Command -> Set ResourceName
reqC ctx (Assignment s) = er ctx (modC ctx (Assignment s)) (varC ctx (Assignment s))
reqC ctx (Sequence c1 c2) = reqC ctx c1 \/ reqC ctx c2
reqC ctx (IfThenElse b c1 c2) = reqC ctx c1 \/ reqC ctx c2 \/ er ctx Set.empty (fv b)
reqC ctx (WhileDo b i c) = reqC ctx c \/ er ctx Set.empty (fv b \/ fv i)
reqC ctx (Call f xs es) =
  reqF ctx fn \/ er ctx (Set.fromList xs) (fv es)
  where fn = getFunction ctx f
reqC ctx (ConcurrentCall f1 r1 v1 f2 r2 v2) =
  reqC ctx (Call f1 r1 v1) \/ reqC ctx (Call f2 r2 v2)
reqC ctx (WithResourceWhen res b c) =
  Set.delete res (reqC ctx c \/ er ctx Set.empty (fv b))

reqF :: Context -> Function -> Set ResourceName
reqF ctx (Function _ refs vals locals (p, c, q)) =
  reqC ctx c \/ er ctx Set.empty (fv p \/ fv q `Set.difference` Set.fromList (refs ++ vals ++ locals))


owned :: Context -> ResourceName -> Set VariableName
owned ctx resourceName =
  Set.fromList resources
  where (Resource _ resources _) = getResource ctx resourceName

var :: Context -> ResourceName -> Set VariableName
var ctx r =
  owned ctx r \/ fv inv
  where (Resource _ _ inv) = getResource ctx r


type ModifiedVariables = Set VariableName
type AccessedVariables = Set VariableName
er :: Context -> ModifiedVariables -> AccessedVariables -> Set ResourceName
er (gamma, _) m a =
  Set.fromList
    [r | (_, Resource r xs inv) <- Map.toList gamma,
         not (null (a /\ Set.fromList xs))
         ||
         not (null (m /\ Set.fromList xs \/ fv inv))]
