module Program where

import Data.Maybe
-- import Data.Set (Set)
-- import qualified Data.Set as Set


-- Type Aliases
type VarName = String
type FunName = String
type FieldName = String
type ResName = String

type Precondition = Assertion
type Postcondition = Assertion
type Invariant = Assertion


-- Data Types
data Command
  = Assign VarName Expression
  | HeapLookup VarName Expression FieldName
  | HeapAssign Expression FieldName Expression
  | New VarName
  | Dispose Expression
  | Block [Command]
  | IfThenElse BoolExpression Command Command
  | While BoolExpression Invariant Command
  | Call ActualArgs
  | ConcurrentCall ActualArgs ActualArgs
  | WithRes ResName BoolExpression Command

type ActualArgs = (FunName, [VarName], [Expression])

data BoolExpression
  = BoolEq Expression Expression
  | BoolNEq Expression Expression
  | BoolTrue | BoolFalse

data Expression
  = Var VarName
  | Nil
  | Const Integer
  | Xor Expression Expression

data Resource = Resource ResName [VarName] Invariant
data Function = Function FunName [VarName] [VarName] [VarName] HoareTriple
-- name, pass-by-ref args, pass-by-value args, local vars, body


-- Assertions
data HeapPredicate
  = PointsTo Expression [(FieldName, Expression)]
  | HeapTree Expression
  | HeapLinkedList Expression Expression
  | HeapXORList Expression Expression Expression Expression


data Assertion
  = AssertIfThenElse BoolExpression Assertion Assertion
  | AssertConj [BoolExpression] [HeapPredicate]

data Program = Program [FieldName] [Resource] [Function]

type HoareTriple = (Precondition, Command, Postcondition)


-- Substitution
class Subst a where
  subst :: [(VarName, VarName)] -> a -> a

substVar :: [(VarName, VarName)] -> VarName -> VarName
substVar m x = fromMaybe x $ lookup x m


instance Subst Expression where
  subst m (Var var) = Var $ substVar m var
  subst _ Nil = Nil
  subst _ (Const c) = Const c
  subst m (Xor e1 e2) = Xor (subst m e1) (subst m e2)

instance Subst BoolExpression where
  subst m (BoolEq e1 e2) = BoolEq (subst m e1) (subst m e2)
  subst m (BoolNEq e1 e2) = BoolNEq (subst m e1) (subst m e2)
  subst _ BoolTrue = BoolTrue
  subst _ BoolFalse = BoolFalse

instance Subst HeapPredicate where
  subst m (PointsTo e heap) = PointsTo (subst m e) (fmap (fmap (subst m)) heap)
  subst m (HeapTree e) = HeapTree (subst m e)
  subst m (HeapLinkedList e e') = HeapLinkedList (subst m e) (subst m e')
  subst m (HeapXORList e1 e2 e3 e4) = HeapXORList (subst m e1) (subst m e2) (subst m e3) (subst m e4)

instance Subst Assertion where
  subst m (AssertIfThenElse be a1 a2) = AssertIfThenElse (subst m be) (subst m a1) (subst m a2)
  subst m (AssertConj bs hs) = AssertConj (map (subst m) bs) (map (subst m) hs)


exampleProgramBody :: Command
--  tree_copy(s;t) [tree(t)] {
--   local i, j, ii, jj;
--   if(t == NULL) s = t;
--   else {
--     i = t->l;
--     j = t->r;
--     tree_copy(ii;i);
--     tree_copy(jj;j);
--     s = new();
--     s->l = ii;
--     s->r = jj;
--   }
-- } [tree(s) * tree(t)]
exampleProgramBody =
  IfThenElse (BoolEq (Var "t") Nil)
    (Assign "s" (Var "t"))
    (Block
       [ HeapLookup "i" (Var "t") "l"
       , HeapLookup "j" (Var "t") "r"
       , Call ("tree_copy", ["ii"], [Var "i"])
       , Call ("tree_copy", ["jj"], [Var "j"])
       , New "s"
       , HeapAssign (Var "s") "l" (Var "ii")
       , HeapAssign (Var "s") "r" (Var "jj")
       ])


-- name, pass-by-ref args, pass-by-value args, local vars, body
exampleProgram :: Program
exampleProgram =
  Program
    ["l", "r"]
    []
    [Function
       "tree_copy" ["s"] ["t"]
       ["i", "j", "ii", "jj"]
       (AssertConj [] [HeapTree (Var "t")],
        exampleProgramBody,
        AssertConj [] [HeapTree (Var "s"), HeapTree (Var "t")])]

