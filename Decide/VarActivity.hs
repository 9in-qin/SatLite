module Decide.VarActivity where

import qualified Data.IntMap as IntMap
import Data.List
import Core.Clause
import Core.Var
import Core.Lit

type VarActivity = IntMap.IntMap Double

conflictBasedUpdate :: Int -> VarActivity -> [Lit] -> VarActivity
conflictBasedUpdate conflictCount varAC learnedCl
    | conflictCount `mod` 100 /= 0 =
        updatedVarAct
    | otherwise =
        IntMap.map (* 0.5) updatedVarAct
    where
        updatedVarAct = updateActivity varAC learnedCl

updateActivity :: VarActivity -> Clause -> VarActivity
updateActivity =
    foldl' updateCertainVar
    where
        updateCertainVar :: IntMap.IntMap Double -> Lit -> IntMap.IntMap Double
        updateCertainVar acc lit = IntMap.insertWith (+) (getVar $ litToVar lit) 1.0 acc