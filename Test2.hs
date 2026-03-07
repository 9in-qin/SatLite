module Test2 where

import qualified Data.Map as Map
import System.Environment (getArgs)
import System.IO
import System.CPUTime
import Text.Printf

import Parser
import Preprocess
import Engine.DPLL as DPLL

main :: IO ()
main = do
    args <- getArgs
    case args of
        [file] -> do
            input <- readFile file
            start <- getCPUTime
            let
                (db, ss) = preprocess $ parse input
                result = DPLL.dpll db ss
            print result
            end <- getCPUTime
            let
                diff = fromIntegral (end - start) / 10^12
            printf "CPU time: %0.3f sec\n" (diff :: Double)
        _      -> putStrLn "CNF file required."