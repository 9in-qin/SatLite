module Core.Restart where

import qualified Data.Map as Map
import qualified Data.IntMap as IntMap
import qualified Data.Sequence as Seq
import Data.List

import Core.ClauseDB
import Core.SolverState
import Preprocess

restart :: ClauseDB -> SolverState -> SolverState
restart db ss =
    ss {assignment = foldl' assignmentConstructor IntMap.empty unitCls,
        level = 0,
        queue = enqueueUnitClauses cls Seq.empty,
        trail = foldl' trailConstructor [] unitCls,
        conflictCount = 0,
        restartThreshold = floor (fromIntegral threshold * 1.25)}
    where
        unitCls = [ l | [l] <- IntMap.elems cls ]
        cls     = clauses db
        threshold = restartThreshold ss