module Decide.Arbitrary where

import qualified Data.Map as Map
import qualified Data.IntMap as IntMap

import Core.Var
import Core.ClauseDB
import Core.SolverState
import Core.Assignment

unAssigned :: VarCount -> Assignment -> Maybe Var
unAssigned totalVar asgmt
    | length asgmt == totalVar =
        Nothing
    | otherwise =
        let asgmt' = IntMap.toList asgmt
            assignedVar = map fst asgmt'
        in Just (Var $ firstUnassigned [1..totalVar] assignedVar)
        where
            firstUnassigned :: [Int] -> [Int] -> Int
            firstUnassigned l1 l2 = head $ filter (`notElem` l2) l1