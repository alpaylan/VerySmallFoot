{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant if" #-}
module SymbolicExecution () where

import Control.Monad.State

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Data.List (find, partition)

import Data.Maybe (isJust)


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






opRules :: SymbolicHoareTriple -> FreshVars [Entailment]
opRules (p, SAssign x e, q) = do
    x' <- fresh
    return [Entailment (subst [(x, x')] p `extendPropAnd` BoolEq (Var x) (subst [(x, x')] e)) q]
opRules (p, SHeapLookup x e f, q) =
    let (rho, p') = p `findAndRemoveEPointsToRho` e in
    let (rho', e') = SymbolicExecution.lookup rho f in do
    x' <- fresh
    return [Entailment (p' `extendPropSep` PointsTo e rho' `extendPropAnd` BoolEq (Var x) (subst [(x, x')] e')) q]
opRules (p, SHeapAssign e f e', q) = 
    let (rho, p') = p `findAndRemoveEPointsToRho` e in
    let rho' = mutate rho f e' in
    let sep = PointsTo e rho' in
    return [Entailment (p' `extendPropSep` sep) q]
opRules (p, SNew x, q) = do
    x' <- fresh
    return [Entailment (subst [(x, x')] p `extendPropSep` PointsTo (Var x) []) q]
opRules (p, SDispose e, q) = 
    return [Entailment (snd (p `findAndRemoveEPointsToRho` e)) q]
opRules (p, SBlock [], q) = 
    return [Entailment p q]
opRules (p, SBlock (x: xs), q) = do
    entailments <- opRules (p, x, q)
    fmap concat . mapM (\(Entailment p' q') -> opRules (p', SBlock xs, q')) $ entailments 
opRules (p, SJump p' xs q', q) = undefined
opRules (p, SIfThenElse b c c', q) = do
    thenCase <- opRules (p `extendPropAnd` b, c, q)
    elseCase <- opRules (p `extendPropAnd` BoolNot b, c', q)
    return $ thenCase ++ elseCase




eagerPartition :: [a] -> (a -> Bool) -> (Maybe a, [a])
eagerPartition ls f = 
    let (ll, lr) = partition f ls in
    case ll of
        [] -> (Nothing, lr)
        (x:xs) -> (Just x, xs ++ lr)



findAndRemoveEPointsToRho :: Prop -> Expression -> (Maybe HeapFields, Prop)
findAndRemoveEPointsToRho (PropIfThenElse b c1 c2) _ = (Nothing, PropIfThenElse b c1 c2)
findAndRemoveEPointsToRho (PropConj pureProp heapProp) e =
    let flatProp = flat heapProp in
    let (ePointsToRho, prop) = eagerPartition flatProp (\case PointsTo e' _ -> e == e'; _ -> False) in
    case ePointsToRho of
        Nothing -> (Nothing, PropConj pureProp heapProp)
        Just (PointsTo _ rho) -> (Just rho, PropConj pureProp (tree prop))
        _ -> error "findAndRemoveEPointsToRho: impossible"
    where
        flat :: HeapProp -> [HeapProp]
        flat (HeapSep p q) = flat p ++ flat q
        flat p = [p]

        tree :: [HeapProp] -> HeapProp
        tree [] = HeapEmp
        tree [x] = x
        tree (x:xs) = HeapSep x (tree xs)



data OperationalRuleApplication
    = OpRuleEmpty Premise
    | OpRuleExpression Premise
    | OpRuleConditional Premise Premise


opRuleApplicable :: SymbolicHoareTriple -> Bool
opRuleApplicable (p, SBlock [], q) = True
opRuleApplicable (p, SBlock (x: xs), q) = opRuleApplicable (p, x, q)
opRuleApplicable (_, SAssign {}, _) = True
opRuleApplicable (_, SNew {}, _) = True
opRuleApplicable (_, SIfThenElse {}, _) = True
opRuleApplicable (p, SHeapLookup _ e _, _) = 
    let (rho, p') = p `findAndRemoveEPointsToRho` e in
    isJust rho
opRuleApplicable (p, SHeapAssign e _ _, _) = 
    let (rho, p') = p `findAndRemoveEPointsToRho` e in
    isJust rho
opRuleApplicable (p, SDispose e, _) = 
    let (rho, p') = p `findAndRemoveEPointsToRho` e in
    isJust rho
opRuleApplicable _ = False


applyOpRule :: SymbolicHoareTriple -> OperationalRuleApplication
applyOpRule (p, SBlock [], q) = OpRuleEmpty (Ent (Entailment p q))
applyOpRule (p, SBlock ((SAssign x e): xs), q) = undefined
applyOpRule (p, SBlock ((SNew x): xs), q) = do
    x' <- fresh
    let p' = (subst [(x, x')] p) `extendPropSep` PointsTo (Var x) [] in
    return $ OpRuleExpression (SymTriple (p', SBlock xs, q))
applyOpRule (p, SBlock ((SIfThenElse b c c'): xs), q) =
    let (p1, p2) = (p `extendPropAnd` b, p `extendPropAnd` BoolNot b) in
    let (q1, q2) = (q, q) in
    let (c1, c2) = (c, c') in
    let (xs1, xs2) = (xs, xs) in
    OpRuleConditional (SymTriple (p1, SBlock (c1: xs1), q1)) (SymTriple (p2, SBlock (c2: xs2), q2))
applyOpRule (p, SBlock ((SHeapLookup x e f): xs), q) = undefined
applyOpRule (p, SBlock ((SHeapAssign e f e'): xs), q) = undefined
applyOpRule (p, SBlock ((SDispose e): xs), q) = undefined
applyOpRule (p, SJump {}, q) = error "applyOpRule: SJump not implemented"
applyOpRule (p, c, q) = applyOpRule (p, SBlock [c], q)





data RearrangementRuleApplication
    = Switch Premise
    | UnrollTree Premise
    | UnrollListSegment Premise

gBeginsWithAofE :: SymbolicHoareTriple -> Bool
gBeginsWithAofE (_, SHeapLookup {}, _) = True
gBeginsWithAofE (_, SHeapAssign {}, _) = True
gBeginsWithAofE (_, SDispose {}, _) = True
gBeginsWithAofE _ = False

getAofE :: SymbolicHoareTriple -> Expression
getAofE (_, SHeapLookup x e f, _) = e
getAofE (_, SHeapAssign e f e', _) = e
getAofE (_, SDispose e, _) = e
getAofE _ = error "getAofE: not applicable"

rearRuleApplicable :: SymbolicHoareTriple -> Bool
rearRuleApplicable (p, SHeapLookup x e f, _) = undefined
rearRuleApplicable (p, SHeapAssign e f e', _) = undefined
rearRuleApplicable (p, SDispose e, _) = undefined
rearRuleApplicable _ = False

applyRearRule :: SymbolicHoareTriple -> RearrangementRuleApplication
applyRearRule (p, SHeapLookup x e f, _) = undefined
applyRearRule (p, SHeapAssign e f e', _) = undefined
applyRearRule (p, SDispose e, _) = undefined
applyRearRule _ = error "applyRearRule: not applicable"




data Entailment = Entailment Prop Prop
data Premise = 
    SymTriple SymbolicHoareTriple 
    | Ent Entailment





incon :: Prop -> Bool
incon p = oracle (Entailment p (PropConj (PropAssert (BoolNEq Nil Nil)) HeapEmp))

oracle :: Entailment -> Bool
oracle = undefined

allocd :: Prop -> Expression -> Bool
allocd p e =
    incon (p `extendPropSep` PointsTo e []) && incon (p `extendPropAnd` BoolEq e Nil)

pre :: SymbolicHoareTriple -> Prop
pre (p, _, _)= p

check :: Premise -> Bool
check (SymTriple g)
    | incon $ pre g = True
    | opRuleApplicable g =
        -- If g matches the conclusion of an operational rule
        case applyOpRule g of 
            OpRuleEmpty p -> case p of
                SymTriple g' -> error "check: impossible"
                Ent e -> oracle e
            OpRuleExpression p -> check p
            OpRuleConditional p1 p2 -> check p1 && check p2
    -- Else if g begins with A(E)
    | gBeginsWithAofE g =
        let e = getAofE g in
        if rearRuleApplicable g then
            let premise = applyRearRule g in
            case premise of
                Switch p -> check p
                UnrollTree p -> check p
                UnrollListSegment p -> check p
    -- elseif allocd(pre(g), E)
        else if allocd (pre g) e then 
            foldr ((&&) . check) True (exor g e)
        else False
check (Ent entailment) = oracle entailment

exor :: SymbolicHoareTriple -> Expression -> Set Premise
exor g e = undefined




