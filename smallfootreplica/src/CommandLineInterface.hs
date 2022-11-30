module CommandLineInterface where
    
import System.Environment
import System.Exit ( ExitCode(ExitFailure, ExitSuccess), exitWith )


-- "smallfoot [options] <file>" where the following command line options
-- are accepted:
--   -verbose         Display additional internal information
--   -all_states      Display intermediate states
--   -show_induction  Indicate when induction is used during verification
--   -very_verbose    Display more additional internal information
--   -help            Display usage message

data Options = Options {
    filePath        :: String,
    verbose         :: Bool,
    allStates       :: Bool,
    showInduction   :: Bool,
    help            :: Bool
} deriving (Show)

commandLineInterface :: IO ()
commandLineInterface = getArgs >>= parse >>= putStrLn


parse :: [String] -> IO String
parse [] = putStrLn "Please provide a file-path for verification" >> exitWith (ExitFailure 1)
parse ls =
    let (help, ls') = "-help" `checkOption` ls in
    let (verbose, ls'') = "-verbose" `checkOption` ls' in
    let (allStates, ls''') = "-all_states" `checkOption` ls'' in
    let (showInduction, ls'''') = "-show_induction" `checkOption` ls''' in
    let filePath = head ls'''' in
    let options = Options filePath verbose allStates showInduction help in
    return $ show options




checkOption :: String -> [String] -> (Bool, [String])
checkOption y ls = checkOption_ [] y ls
    where
        checkOption_ :: [String] -> String -> [String] ->  (Bool, [String])
        checkOption_ acc _ [] = (False, acc)
        checkOption_ acc y (x:xs) = if x == y 
                                    then (True, xs ++ acc) 
                                    else checkOption_ (x:acc) y xs

