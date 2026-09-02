module Main where

import System.Environment
import System.CPUTime
import Text.Printf
import Control.Exception

import Preprocess.Parser
import Preprocess.Preprocess
import Engine.CDCL as CDCL

main :: IO ()
main = do
    args <- getArgs
    case args of
        [file] -> do
            input <- readFile file
            start <- getCPUTime
            case parse input of
                Nothing -> do
                    end <- getCPUTime
                    putStrLn "UNSAT"
                    let cpuTime = fromIntegral (end - start) / 10 ^ 12
                    printf "CPU time: %0.6f sec\n" (cpuTime :: Double)
                Just parsingResult -> do
                    let (db, ss) = preprocess parsingResult
                    result <- evaluate (CDCL.cdcl db ss)
                    end    <- getCPUTime
                    print result
                    let cpuTime = fromIntegral (end - start) / 10 ^ 12
                    printf "CPU time: %0.6f sec\n" (cpuTime :: Double)
        _ -> putStrLn "CNF file required"