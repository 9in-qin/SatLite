module Decide.VSIDS where

import qualified Data.Map as Map
import qualified Data.IntMap as IntMap
import qualified Data.Set as Set
import Data.List

import Core.Var
import Core.ClauseDB
import Core.SolverState
import Core.Assignment

mostActiveVar :: VarCount -> Assignment -> VarActivity -> Maybe Var
mostActiveVar totalVar asgmt varAC
    | length asgmt == totalVar =
        Nothing
    | otherwise =
        Just $ Var (fst nextVar)--(head notAssigned)
        where
            nextVar = IntMap.foldlWithKey' mostActive (0, 0.0) varAC
            mostActive (var, score) var' score' =
                case IntMap.lookup var' asgmt of
                    Nothing -> if score' > score then (var', score') else (var, score)
                    _       -> (var, score)