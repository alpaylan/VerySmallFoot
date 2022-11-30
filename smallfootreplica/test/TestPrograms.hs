module TestPrograms 
    (
        treeCopy
        , treeCopyBlock
        , heapRace
    ) where

import Program



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
treeCopy :: Program
treeCopy =
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

treeCopyBlock :: Command
treeCopyBlock =  Block
        [ HeapLookup "i" (Var "t") "l"
        , HeapLookup "j" (Var "t") "r"
        , Call ("tree_copy", ["ii"], [Var "i"])
        , Call ("tree_copy", ["jj"], [Var "j"])
        , New "s"
        , HeapAssign (Var "s") "l" (Var "ii")
        , HeapAssign (Var "s") "r" (Var "jj")
        ]




-- tl;

-- update(x,y) [x|->] {
--   x->tl = y;
-- } [x|->y]

-- heap_race() {
--   c = new();
--   update(c,42) || update(c,13);
-- } [c|->]
    
heapRace :: Program
heapRace = 
    Program
    ["tl"]
    []
    [Function
       "update" ["x"] ["y"]
       []
       (AssertConj [] [PointsTo (Var "x") []]
       ,HeapAssign (Var "x") "tl" (Var "y")
       ,AssertConj [] [PointsTo (Var "x") [("tl", Var "y")]])
    , Function
        "heap_race" [] []
        []
        (AssertConj [] [],
        Block
            [ New "c"
            , ConcurrentCall ("update", ["c"], [Const 42]) ("update", ["c"], [Const 13])
            ]
        , AssertConj [] [PointsTo (Var "c") []])]