module Decide.VSIDS where

import qualified Data.IntMap as IntMap

import Core.Assignment
import Core.Var
import Decide.VarActivity

mostActiveVar :: Assignment -> VarActivity -> Maybe Var
mostActiveVar asgmt varAC
    | mostActVar == -1 =
        Nothing
    | otherwise =
        Just $ Var mostActVar
        where
            (mostActVar, _) = IntMap.foldlWithKey' mostActive (-1, - (1 / 0)) varAC

            mostActive curMost@(var, score) var' score'
                | IntMap.member var' asgmt = curMost
                | score' > score = (var', score')
                | otherwise = curMost