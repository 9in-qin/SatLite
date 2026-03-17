module MainOld where

import qualified Data.Map as Map
import System.Environment (getArgs)
import System.IO

import Parser
import Core.Types
import Preprocess
import Engine.DPLL
import Engine.CDCL

main :: IO ()
main = do
    args <- getArgs
    case args of
        [file] -> do
            input <- readFile file
            let
                processedClauses = preprocess $ parse input
                db = fst processedClauses
                ss = snd processedClauses
            -- print $ dpll db ss
            -- print $ cdcl db ss
            print db
            print ss
        _      -> putStrLn "CNF file required."