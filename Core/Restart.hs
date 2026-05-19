module Core.Restart where

import qualified Data.Map as Map
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
    ss { assignment       = foldl' assignmentConstructor IntMap.empty unitCls
       , queue            = enqueueUnitClauses cls Seq.empty
       , trail            = foldl' trailPush emptyTrail varAndRsn
       , conflictCount    = 0
       , restartThreshold = floor (fromIntegral threshold * 1.25)
       }
    where
        unitCls   = [ l | [l] <- IntMap.elems cls ]
        unitLits  = [ (cid, l) | (cid, [l]) <- IntMap.toList cls ]
        unitLits' = map snd unitLits
        unitVars  = map (fmap litToVar) unitLits
        varAndRsn = map (\(x, y) -> (y, Propagated x)) unitVars
        cls       = clauses db
        threshold = restartThreshold ss