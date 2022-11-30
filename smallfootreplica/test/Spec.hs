
import VariableConditions
import qualified Data.Set as Set
import Test.HUnit
import TestPrograms

checkVarC :: Test
checkVarC =
    TestCase $ assertEqual 
    "All variables in the command should be in the set" 
    (varC (mkContext treeCopy) treeCopyBlock) 
    (Set.fromList ["i", "ii", "j", "jj", "s", "t"])

checkModC :: Test
checkModC =
    TestCase $ assertEqual
    "All variables modified in the command should be in the set"
    (modC (mkContext treeCopy) treeCopyBlock)
    (Set.fromList ["i", "ii", "j", "jj", "s"])
 
checkReqC :: Test
checkReqC =
    TestCase $ assertEqual
    "All resources required in the command should be in the set"
    (reqC (mkContext treeCopy) treeCopyBlock)
    Set.empty

checkParTreeCopy :: Test
checkParTreeCopy =
    TestCase $ assertEqual
    "All functions called in parallel with tree_copy must be in the set"
    (par (mkContext treeCopy) "tree_copy")
    Set.empty

checkParHeapRace :: Test
checkParHeapRace =
    TestCase $ assertEqual
    "All functions called in parallel with update must be in the set"
    (par (mkContext heapRace) "update") 
    (Set.fromList ["update"])

main :: IO ()
main = runTestTT (TestList [checkVarC, checkModC, checkReqC, checkParTreeCopy, checkParHeapRace]) >> return ()