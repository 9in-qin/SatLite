module Core.Restart where

import qualified Data.IntMap as IntMap
import qualified Data.Sequence as Seq
import Data.List

import Core.ClauseDB
import Core.SolverState
import Preprocess
import Core.Trail
import Core.Lit

restart :: ClauseDB -> SolverState -> SolverState
restart db ss =
    ss { assignment       = foldl' assignmentConstructor IntMap.empty unitLits
       , queue            = enqueueUnitClauses unitLits Seq.empty
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