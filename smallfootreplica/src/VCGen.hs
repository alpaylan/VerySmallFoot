module VCGen where

import Data.Set (Set)
import qualified Data.Set as Set

import Program
import VariableConditions


newtype JsrInstruction = JsrInstruction FunctionHeader deriving (Eq, Ord)

data SymbolicInstruction = 
    Eps
    | SymbolicAssignment Assignment
    | SymbolicHoareTriple Precondition JsrInstruction Postcondition
    | SymbolicIfThenElse BoolExpression SymbolicInstruction SymbolicInstruction
    | SymbolicSequence SymbolicInstruction SymbolicInstruction
    deriving (Eq, Ord)
data SymbolicProcedureBody = SymbolicProcedureBody  {
                                                    si_precondition :: Precondition
                                                    , si :: SymbolicInstruction
                                                    , si_postcondition :: Postcondition
                                                    } deriving (Eq, Ord)



vcg :: FunctionName -> HoareTriple -> Set SymbolicProcedureBody

vcg g pcq =
    let HoareTriple {precondition = p , command = c, postcondition = q }  =  pcq in
    let (si, l) = chop g c in
        SymbolicProcedureBody p si q `Set.insert` l 


chop :: FunctionName -> Command -> (SymbolicInstruction, Set SymbolicProcedureBody)

chop g (Assignment s) = (SymbolicAssignment s, Set.empty)
chop g (Sequence c1 c2) = 
    let (si1, l1) = chop g c1 in
    let (si2, l2) = chop g c2 in
        (SymbolicSequence si1 si2, l1 `Set.union` l2)
chop g (IfThenElse b c1 c2) =
    let (si1, l1) = chop g c1 in
    let (si2, l2) = chop g c2 in
        (SymbolicIfThenElse b si1 si2, l1 `Set.union` l2)
chop _ _ = (Eps, Set.empty)


