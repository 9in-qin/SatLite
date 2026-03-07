module Parser where

import Data.List
import Data.Maybe

parse :: String -> ([[Int]], (Int, Int))
parse input = 
        let
            allLines = lines input
            headerLine =
                case find isHeaderLine allLines of
                    Just line -> line
                    Nothing   -> error "No header line found"
            clauseLines = filter isClauseLine allLines
        in  (map purify clauseLines, parseHeaderLine headerLine)
        where
            isHeaderLine :: String -> Bool
            isHeaderLine line = "p cnf" `isPrefixOf` line

            parseHeaderLine :: String -> (Int, Int)
            parseHeaderLine line =
                case words line of
                    ("p":"cnf":numVars:numClauses:_) ->
                        (read numVars, read numClauses)
                    _                                ->
                        error "Invalid header line format"

            isClauseLine :: String -> Bool
            isClauseLine line = not ("c" `isPrefixOf` line || "p cnf" `isPrefixOf` line)

            purify :: String -> [Int]
            purify line = map read $ init $ words line