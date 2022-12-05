module Main (main) where

import Program
import Parser
import VCGen
import SymbolicExecution

main :: IO ()
main = parseProgramFile "test/business1.sf" >>= either print (print . checkProgram)
    where checkProgram = map (check . SymTriple) . concat . generateSymbolicProgram
