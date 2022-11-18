
module VariableConditions
    (
        varC,
        varF,
        modC,
        modF,
        reqC,
        reqF
    )
    where
        
import Program

import Data.Set (Set)
import qualified Data.Set as Set

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
fvhr (HeapRecord f e) = fv e

fvas :: Assignment -> Set VariableName
fvas (VariableAssignment x e) = fv x \/ fv e
fvas (HeapLookup x e t) = fv x \/ fv e \/ fv t
fvas (HeapMutation e t x) = fv x \/ fv e \/ fv t
fvas (Allocation x) = fv x
fvas (Deallocation e) = fv e

fvc :: Command -> Set VariableName
fvc (Assignment assignment) = fv assignment
fvc (IfThenElse b c1 c2) = fv b \/ fv c1 \/ fv c2
fvc (WhileDo b i c) = fv b \/ fv i \/ fv c
fvc (Sequence c1 c2) = fv c1 \/ fv c2
fvc (FunctionCall (FunctionHeader f p v)) = Set.unions $ map fv (p ++ v)




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
instance FV Identifier where
    fv = Set.singleton
instance FV Command where
    fv = fvc
instance FV Assignment where
    fv = fvas




data Context = Context  {
                        gamma :: Set Function
                        , delta :: Set Resource
                        }

functionName :: Function -> FunctionName
functionName (Function (FunctionHeader name _ _) _ ) = name


getFunction :: Context -> FunctionName -> Function
getFunction ctx fname = 
    let functionSet = gamma ctx in
    let function = Set.filter (\f -> fname == functionName f) functionSet in
        if Set.size function == 1 then
            Set.elemAt 0 function
        else
            error "Function not found"

getResource :: Context -> ResourceName -> Resource
getResource ctx rname = 
    let resourceSet = delta ctx in
    let resource = Set.filter (\r -> rname == resourceName r) resourceSet in
        if Set.size resource == 1 then
            Set.elemAt 0 resource
        else
            error "Resource not found"


varC :: Context -> Command -> Set VariableName
varC _ (Assignment (VariableAssignment var expression)) = 
    Set.singleton var \/ fv expression
varC _ (Assignment (HeapLookup var expression _)) =
    Set.singleton var \/ fv expression
varC _ (Assignment (HeapMutation expression _ expression')) =
    fv expression \/ fv expression'
varC _ (Assignment (Allocation var)) = Set.singleton var
varC _ (Assignment (Deallocation expression)) = fv expression
varC ctx (Sequence c1 c2) = varC ctx c1 \/ varC ctx c2
varC ctx (IfThenElse b c1 c2) = fv b \/ varC ctx c1 \/ varC ctx c2
varC ctx (WhileDo b i c) = (fv b `Set.intersection` fv i) \/ varC ctx c
varC ctx (FunctionCall (FunctionHeader id p v)) = 
    let f = getFunction ctx id in
        varF ctx f  \/ Set.fromList p \/ fv v
varC ctx(ConcurrentFunctionCall function1 function2) =
    varC ctx (FunctionCall function1) \/ varC ctx (FunctionCall function2)
varC ctx (WithResourceWhen resource b c) =
    let r = getResource ctx resource in
        ((fv b \/ varC ctx c) `Set.difference` fv c) \/ (modC ctx c `Set.difference` owned ctx resource)

varF :: Context -> Function -> Set VariableName
varF ctx (Function (FunctionHeader _ pointers values) (HoareTriple {precondition = p, command = c, postcondition = q})) = 
    (varC ctx c \/ fv p \/ fv q) `Set.difference` (Set.fromList pointers \/ Set.fromList values)


    
    

modC :: Context -> Command -> Set VariableName
modC _ (Assignment (VariableAssignment var _)) = Set.singleton var
modC _ (Assignment (HeapLookup var _ _)) = Set.singleton var
modC _ (Assignment (HeapMutation expression _ var)) = fv var \/ fv expression
modC _ (Assignment (Allocation var)) = Set.singleton var
modC _ (Assignment (Deallocation expression)) = fv expression
modC ctx (Sequence c1 c2) = modC ctx c1 \/ modC ctx c2
modC ctx (IfThenElse b c1 c2) = modC ctx c1 \/ modC ctx c2
modC ctx (WhileDo b i c) = modC ctx c
modC ctx (FunctionCall (FunctionHeader id p _)) =
    let f = getFunction ctx id in
        modF ctx f \/ Set.fromList p
modC ctx (ConcurrentFunctionCall function1 function2) =
    modC ctx (FunctionCall function1) \/ modC ctx (FunctionCall function2)
modC ctx (WithResourceWhen resource b c) =
    modC ctx c `Set.difference` owned ctx resource

    

modF :: Context -> Function -> Set VariableName
modF ctx (Function (FunctionHeader _ pointers values) (HoareTriple {command = c})) = 
    modC ctx c `Set.difference` (Set.fromList pointers \/ Set.fromList values)


reqC :: Context -> Command -> Set ResourceName
reqC ctx (Assignment s) = er ctx (modC ctx (Assignment s)) (varC ctx (Assignment s))
reqC ctx (Sequence c1 c2) = reqC ctx c1 \/ reqC ctx c2
reqC ctx (IfThenElse b c1 c2) = reqC ctx c1 \/ reqC ctx c2 \/ er ctx Set.empty (fv b)
reqC ctx (WhileDo b i c) = reqC ctx c \/ er ctx Set.empty (fv b \/ fv i)
reqC ctx (FunctionCall (FunctionHeader id p v)) = 
    let f = getFunction ctx id in
        reqF ctx f \/ er ctx (Set.fromList p) (fv v)
reqC ctx (ConcurrentFunctionCall function1 function2) =
    reqC ctx (FunctionCall function1) \/ reqC ctx (FunctionCall function2)
reqC ctx (WithResourceWhen resource b c) = 
    (reqC ctx c \/ er ctx Set.empty (fv b)) `Set.difference` Set.singleton resource

reqF :: Context -> Function -> Set ResourceName
reqF ctx (Function (FunctionHeader _ pointers values) (HoareTriple {precondition = p, command = c, postcondition = q})) = 
    reqC ctx c \/ er ctx Set.empty (fv p \/ fv q `Set.difference` (Set.fromList pointers \/ Set.fromList values))


owned :: Context -> ResourceName -> Set VariableName
owned ctx resourceName = 
    let (Resource _ resources _) = getResource ctx resourceName in
        Set.fromList resources

var :: Context -> ResourceName -> Set VariableName
var ctx resourceName = 
    let (Resource _ resources resourceInvariant) = getResource ctx resourceName in
        owned ctx resourceName \/ fv resourceInvariant
    




type ModifiedVariables = Set VariableName
type AccessedVariables = Set VariableName
er :: Context -> ModifiedVariables -> AccessedVariables -> Set ResourceName
er ctx m a = 
    let resourceSet = delta ctx in
    Set.map resourceName (Set.filter(\r -> 
        (a /\ Set.fromList (resources r) /= Set.empty)
        ||
        (m /\ (Set.fromList (resources r) \/ fv (resourceInvariant r)) /= Set.empty)
        ) resourceSet)

