module Decide.VarActivity where

import qualified Data.IntMap as IntMap
import Data.List
import Core.Clause
import Core.Var
import Core.Lit

type VarActivity = IntMap.IntMap Double

--VSIDS related functions
updateActivity :: VarActivity -> Clause -> VarActivity
updateActivity = --foldl' updateCertainVar
    foldl' (\ac (Lit i) -> IntMap.insertWith (+) (getVar $ litToVar (Lit i)) 1.0 ac)
    -- where
    --     updateCertainVar :: Map.Map Var Double -> Lit -> Map.Map Var Double
    --     updateCertainVar ac (Lit i) = Map.insertWith (+) (Var i) 1.0 ac