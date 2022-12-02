module SymbolicExecution () where

import Control.Monad.State

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Program
import VCGen
import VariableConditions

type HeapFields = [(FieldName, Expression)]


mutate :: HeapFields -> FieldName -> Expression -> HeapFields
mutate p f e =
    case Map.lookup f $ Map.fromList p of
        Nothing -> (f, e) : p
        Just _ -> map (\(f', e') -> if f == f' then (f, e) else (f', e')) p
    

lookup ::  HeapFields -> FieldName -> (HeapFields, Expression)
lookup p f = 
    evalState (lookup' p f) (fvs "_lfv")
    where
    lookup' :: HeapFields -> FieldName -> FreshVars (HeapFields, Expression)
    lookup' p f =
        case Map.lookup f $ Map.fromList p of
            Nothing -> 
                do
                v <- fresh
                return ((f, Var v):p, Var v)
            Just e -> 
                return (p, e)


opRules :: Context  -> SymbolicHoareTriple -> [Entailment]
opRules ctx (p, SAssign x e, q) = do
    x' <- fresh
    return $ (p `subst` (x, x')) `extendPropAnd` BoolEq (Var x) (e `subst` (x, x'))
opRules ctx (p, SHeapLookup x e f, q) = undefined
opRules ctx (p, SHeapAssign e f e', q) = undefined  
opRules ctx (p, SNew x, q) = do
    x' <- fresh
    return $ (p `subst` (x, x')) `extendPropSep` PointsTo (Var x) []
opRules ctx (p, SDispose e, q) = 
    return $ p `removeHeapElem` e
    where
        removeHeapElem :: Prop -> Expression -> Prop
        removeHeapElem (PropConj pureProp heapProp) e = 
            PropConj pureProp $ removeHeapElem' (flat heapProp) e
        removeHeapElem' :: [HeapProp] -> Expression -> HeapProp
        removeHeapElem' (PointsTo e' _) e = 
            if e == e' then HeapPropTrue else PointsTo e' []






opRules ctx (p, SBlock [], q) = [Entailment p q]
opRules ctx (p, SBlock (x: xs), q) = 
    let entailments = opRules ctx (p, x, q) in
        map (\(Entailment p' q') -> opRules ctx (p', SBlock xs, q')) entailments
opRules ctx (p, SJump p' xs q', q) = undefined
opRules ctx (p, SIfThenElse b c c', q) =
    let thenCase = opRules ctx (p `extendPropAnd` b, c, q) in
    let elseCase = opRules ctx (p `extendPropAnd` BoolNot b, c', q) in
        thenCase ++ elseCase

data Entailment = Entailment Prop Prop


flat :: HeapProp -> [HeapProp]
flat (SepConj p q) = flat p ++ flat q
flat p = [p]


tree :: [HeapProp] -> HeapProp
tree [] = Emp
tree [x] = x
tree (x:xs) = SepConj x (tree xs)
