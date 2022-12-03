module Main (main) where

import Program
import Parser
import VCGen
import SymbolicExecution

main :: IO ()
main = do
    program <- Parser.parseProgramFile "test/business1.sf" 
    case program of
        Left err -> print err
        Right p ->
            let symbolicProgram = concat $ generateSymbolicProgram p in
            let verificationResults = map (\p -> SymbolicExecution.check (SymTriple p)) symbolicProgram in
            print verificationResults
            
    -- let symbolicProgram = VCGen.generateSymbolicProgram program
    -- print symbolicProgram
    
