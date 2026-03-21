module Decide.VSIDS where

import qualified Data.Map as Map
import qualified Data.IntMap as IntMap
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
            nextVar = Map.foldlWithKey' mostActive (Var 0, 0.0) varAC
            mostActive (var, score) var' score' =
                case IntMap.lookup (getVar var') asgmt of
                    Nothing -> if score' > score then (var', score') else (var, score)
                    _       -> (var, score)