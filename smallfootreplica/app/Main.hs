module Main (main) where

import Program
import Parser


main :: IO ()
main = do
    program <-Parser.parseProgramFile "test/business1.sf" 
    print program
    
