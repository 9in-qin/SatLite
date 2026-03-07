module Decide.VSIDS where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List

import Core.Types
import Core.VarLit

mostActiveVar :: VarCount -> Assignment -> VarActivity -> Maybe Var
mostActiveVar totalVar asgmt varAC
    | length asgmt == totalVar =
        Nothing
    | otherwise =
        Just $ fst nextVar--(head notAssigned)
        where
            asgmt' = Map.toList asgmt
            assignedVar = map fst asgmt'
            notAssigned = [Var i | i <- [1..totalVar], Var i `notElem` assignedVar]
            nextVar = Map.foldlWithKey' mostActive (Var 0, 0.0) (Map.restrictKeys varAC (Set.fromList notAssigned))
            mostActive (var, score) var' score' = if score' > score then (var', score') else (var, score)