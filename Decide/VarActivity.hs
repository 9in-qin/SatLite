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
        updateActivity varAC learnedCl
    | otherwise =
        IntMap.map (* 0.5) $ updateActivity varAC learnedCl

updateActivity :: VarActivity -> Clause -> VarActivity
updateActivity = foldl' updateCertainVar
    --foldl' (\ac (Lit i) -> IntMap.insertWith (+) (getVar $ litToVar (Lit i)) 1.0 ac)
    where
        updateCertainVar :: IntMap.IntMap Double -> Lit -> IntMap.IntMap Double
        updateCertainVar ac lit = IntMap.insertWith (+) (getVar $ litToVar lit) 1.0 ac