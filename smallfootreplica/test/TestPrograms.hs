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
       "tree_copy" ["s"] ["t"] ["i", "j", "ii", "jj"]
       (PropConj PropTrue (HeapTree (Var "t"))
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
        , PropConj PropTrue (HeapSep (HeapTree (Var "s")) (HeapTree (Var "t"))))]

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
       (PropConj PropTrue (PointsTo (Var "x") [])
       ,HeapAssign (Var "x") "tl" (Var "y")
       ,PropConj PropTrue (PointsTo (Var "x") [("tl", Var "y")]))
    , Function
        "heap_race" [] []
        []
        (PropConj PropTrue HeapEmp,
        Block
            [ New "c"
            , ConcurrentCall ("update", ["c"], [Const 42]) ("update", ["c"], [Const 13])
            ]
        , PropConj PropTrue (PointsTo (Var "c") []))]





-- /* Combining the pointer-transferring buffer and the memory manager */

-- tl;

-- init() { f = NULL; c = NULL; }

-- resource mm (f) [List(f)]

-- alloc(x;) {
--     with mm when(true) {
--     if(f==NULL) x = new();
--     else { x = f; f = x->tl; }}
-- } [x|->]

-- dealloc(y) [y|->] {
--     with mm when(true) { y->tl = f; f = y; }
-- }

-- resource buf (c) [if c==NULL then emp else c|->]

-- put(x) [x|->] {
--     with buf when (c==NULL) { c = x; }
-- } [emp]

-- get(y;) [emp] {
--     with buf when (c!=NULL) { y = c; c = NULL; }
-- } [y|->]

-- putter() {
--     local x;
--     alloc(x;);
--     put(x);
--     putter();
-- }

-- getter() {
--     local y;
--     get(y;);
--     /* use y */
--     dealloc(y);
--     getter();
-- }

-- main() {
--     putter() || getter();
-- }


mmBuf :: Program
mmBuf =
    Program
    ["tl"]
    -- Resources
    [-- resource mm (f) [List(f)]
    Resource "mm" ["f"] (PropConj PropTrue (HeapListSegment (Var "f") Nil))
    -- resource buf (c) [if c==NULL then emp else c|->]
    , Resource "buf" ["c"] (PropIfThenElse (PropAssert (BoolEq (Var "c") Nil)) (PropConj PropTrue HeapEmp) (PropConj PropTrue (PointsTo (Var "c") [])))]
    -- Functions
    [
    -- init() { f = NULL; c = NULL; }
    Function
       "init" [] [] []
       (PropConj PropTrue HeapEmp,
       Block
           [ Assign "f" Nil
           , Assign "c" Nil
           ]
       , PropConj PropTrue HeapEmp)
    -- alloc(x;) {
    --     with mm when(true) {
    --     if(f==NULL) x = new();
    --     else { x = f; f = x->tl; }}
    -- } [x|->]
    , Function
         "alloc" ["x"] [] []
         (
            PropConj PropTrue HeapEmp
            , WithRes "mm" BoolTrue
                (IfThenElse (BoolEq (Var "f") Nil)
                    (New "x")
                    (Block
                        [ Assign "x" (Var "f")
                        , HeapLookup "f" (Var "x") "tl"]
                    ))
            , PropConj PropTrue (PointsTo (Var "x") [])
         )
    -- dealloc(y) [y|->] {
    --     with mm when(true) { y->tl = f; f = y; }
    -- }
    , Function
         "dealloc" ["y"] [] []
         (
            PropConj PropTrue (PointsTo (Var "y") [])
            , WithRes "mm" BoolTrue
                (Block
                    [ HeapAssign (Var "y") "tl" (Var "f")
                    , Assign "f" (Var "y")
                    ]
                )
            , PropConj PropTrue HeapEmp
         )
    -- put(x) [x|->] {
    --     with buf when (c==NULL) { c = x; }
    -- } [emp]
    , Function
         "put" ["x"] [] []
         (
            PropConj PropTrue (PointsTo (Var "x") [])
            , WithRes "buf" (BoolEq (Var "c") Nil)
                (Assign "c" (Var "x"))
            , PropConj PropTrue HeapEmp
         )
    -- get(y;) [emp] {
    --     with buf when (c!=NULL) { y = c; c = NULL; }
    -- } [y|->]
    , Function
         "get" ["y"] [] []
         (
            PropConj PropTrue HeapEmp
            , WithRes "buf" (BoolNEq (Var "c") Nil)
                (Block
                    [ Assign "y" (Var "c")
                    , Assign "c" Nil
                    ]
                )
            , PropConj PropTrue (PointsTo (Var "y") [])
         )
    -- putter() {
    --     local x;
    --     alloc(x;);
    --     put(x);
    --     putter();
    -- }
    , Function
        "putter" [] [] ["x"]
        (PropConj PropTrue HeapEmp,
        Block
            [ Call ("alloc", ["x"], [])
            , Call ("put", ["x"], [])
            , Call ("putter", [], [])
            ]
        , PropConj PropTrue HeapEmp)
    -- getter() {
    --     local y;
    --     get(y;);
    --     /* use y */
    --     dealloc(y);
    --     getter();
    -- }
    , Function
        "getter" [] [] ["y"]
        (PropConj PropTrue HeapEmp,
        Block
            [ Call ("get", ["y"], [])
            , Call ("dealloc", ["y"], [])
            , Call ("getter", [], [])
            ]
        , PropConj PropTrue HeapEmp)
    -- main() {
    --     putter() || getter();
    -- }
    , Function
        "main" [] [] []
        (PropConj PropTrue HeapEmp,
        Block
            [ ConcurrentCall ("putter", [], []) ("getter", [], [])
            ]
        , PropConj PropTrue HeapEmp)
    ]