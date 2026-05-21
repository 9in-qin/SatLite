module Parser where

import Data.List

parse :: String -> Maybe ([[Int]], (Int, Int))
parse input =
    let allLines    = lines input
        headerLine  =
            case find isHeaderLine allLines of
                Just line -> line
                Nothing   -> error "No header line found"
        headerInfo  = parseHeaderLine headerLine
        clauseLines = filter clauseLineOnly allLines
        clauses     = map (stringToInts . init) clauseLines
        ifEmptyCl   = any null clauses
    in (if ifEmptyCl then Nothing else Just (clauses, headerInfo))
    where
        isHeaderLine :: String -> Bool
        isHeaderLine line =
            "p cnf" `isPrefixOf` line

        isCommentLine :: String -> Bool
        isCommentLine line =
            "c" `isPrefixOf` line

        isClauseLine :: String -> Bool
        isClauseLine line =
            "0" `isSuffixOf` line

        clauseLineOnly :: String -> Bool
        clauseLineOnly line =
            all ($ line) [not . isCommentLine, isClauseLine]

        stringToInts :: String -> [Int]
        stringToInts line =
            map read (words line)

        parseHeaderLine :: String -> (Int, Int)
        parseHeaderLine line =
            case words line of
                ("p" : "cnf" : numVars : numClauses: _) ->
                    (read numVars, read numClauses)
                _ ->
                    error "Invalid header line format"