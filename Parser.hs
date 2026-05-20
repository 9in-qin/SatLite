module Parser where

import Data.List

parse :: String -> ([[Int]], (Int, Int))
parse input = 
    let allLines = lines input
        headerLine =
            case find isHeaderLine allLines of
                Just line -> line
                Nothing   -> error "No header line found"
        clauseLines = map init $ filter clauseLineOnly allLines
    in (map stringToInt clauseLines, parseHeaderLine headerLine)
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
        
        isEmptyLine :: String -> Bool
        isEmptyLine "" = True
        isEmptyLine _ = False

        clauseLineOnly :: String -> Bool
        clauseLineOnly line =
            all ($ line) [isClauseLine, not . isCommentLine, not . isEmptyLine]

        stringToInt :: String -> [Int]
        stringToInt line =
            map read (words line)

        parseHeaderLine :: String -> (Int, Int)
        parseHeaderLine line =
            case words line of
                ("p" : "cnf" : numVars : numClauses:_) ->
                    (read numVars, read numClauses)
                _ -> error "Invalid header line format"