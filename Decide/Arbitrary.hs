module Decide.Arbitrary where

import qualified Data.Map as Map

import Core.Types
import Core.VarLit

unAssigned :: VarCount -> Assignment -> Maybe Var
unAssigned totalVar asgmt
    | length asgmt == totalVar =
        Nothing
    | otherwise =
        let asgmt' = Map.toList asgmt
            assignedVar = map (getVar . fst) asgmt'
        in Just (Var $ firstUnassigned [1..totalVar] assignedVar)
        where
            firstUnassigned :: [Int] -> [Int] -> Int
            firstUnassigned l1 l2 = head $ filter (`notElem` l2) l1