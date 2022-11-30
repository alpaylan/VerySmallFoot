
import Program
import VariableConditions
import qualified Data.Set as Set



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

-- name, pass-by-ref args, pass-by-value args, local vars, body
exampleProgram :: Program
exampleProgram =
  Program
    ["l", "r"]
    []
    [Function
       "tree_copy" ["s"] ["t"]
       ["i", "j", "ii", "jj"]
       (AssertConj [] [HeapTree (Var "t")]
       ,IfThenElse (BoolEq (Var "t") Nil)
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
        , AssertConj [] [HeapTree (Var "s"), HeapTree (Var "t")])]



comm :: Command
comm =  Block
        [ HeapLookup "i" (Var "t") "l"
        , HeapLookup "j" (Var "t") "r"
        , Call ("tree_copy", ["ii"], [Var "i"])
        , Call ("tree_copy", ["jj"], [Var "j"])
        , New "s"
        , HeapAssign (Var "s") "l" (Var "ii")
        , HeapAssign (Var "s") "r" (Var "jj")
        ]



checkVarC :: Bool
checkVarC =
    varC (mkContext exampleProgram) comm == Set.fromList ["i", "ii", "j", "jj", "s", "t"]


checkModC :: Bool
checkModC =
    modC (mkContext exampleProgram) comm == Set.fromList ["i", "ii", "j", "jj", "s"]

        
checkReqC :: Bool
checkReqC =
    reqC (mkContext exampleProgram) comm == Set.fromList []



main :: IO ()
main = print checkVarC >> print checkModC >> print checkReqC