module Core.Restart where

import qualified Data.IntMap as IntMap
import Data.List

import Core.Assignment
import Core.ClauseDB
import Core.Queue
import Core.SolverState
import Core.Trail
import Core.Lit

restart :: ClauseDB -> SolverState -> SolverState
restart db ss =
    ss { assignment       = foldl' assignLiteral emptyAssignment unitLits
       , queue            = enqueueUnitClauses unitLits emptyQueue
       , trail            = foldl' trailPush emptyTrail varAndRsn
       , conflictCount    = 0
       , restartThreshold = floor (fromIntegral threshold * 1.25)
       }
    where
        cls       = clauses db
        threshold = restartThreshold ss
        unitCls   = [ (cid, l) | (cid, [l]) <- IntMap.toList cls ]
        unitLits  = map snd unitCls
        unitVars  = map (fmap litToVar) unitCls
        varAndRsn = map (\(x, y) -> (y, Propagated x)) unitVars