module SymbolicExecution () where

import Control.Monad.State

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Program
import VCGen (FreshVars)
import VariableConditions

type HeapFields = [(FieldName, Expression)]


mutate :: HeapFields -> FieldName -> Expression -> HeapFields
mutate p f e =
    case Map.lookup f $ Map.fromList p of
        Nothing -> (f, e) : p
        Just _ -> map (\(f', e') -> if f == f' then (f, e) else (f', e')) p
    

lookup ::  Context -> HeapFields -> FieldName -> FreshVars (HeapFields, Expression)
lookup ctx p f =
    case Map.lookup f $ Map.fromList p of
        Nothing -> do
            v <- fresh
            return ((f, Var v):p, Var v)
        Just e -> return (p, e)




    







