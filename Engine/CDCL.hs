module Engine.CDCL where

import qualified Data.Map as Map
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Sequence as Seq
import Data.List

import Core.Var
import Core.Lit
import Core.Clause
import Core.Queue
import Core.Trail
import Core.Watched
import Core.Restart
import Core.ClauseDB
import Core.SolverState
import Decide.Arbitrary
import Decide.VSIDS
import Core.Assignment
import Decide.VarActivity
import Engine.Analyze
import Engine.Backjump

data Result        = UNSAT | SAT (ClauseDB, SolverState) deriving (Show)

cdcl :: ClauseDB -> SolverState -> Result
cdcl db ss =
    let propagation = propagate db ss
    in case hasConflict propagation of
        (db', ss', Just cid) -> case level ss' of -- ss or ss'? as the level has not changed yet (ss' better)
            0  -> UNSAT
            lv -> let (conflictClsToVar, currentLevelVars, rsns) = extractInfo (db', ss', cid)
                      (learnedClause, firstUIP) = analyze (conflictClsToVar, currentLevelVars, rsns) currentLevelVars (clauses db')
                      learnedClause' = IntMap.elems learnedClause
                      (newDB, newSS) = backjump db' ss' learnedClause' firstUIP lv
                      newSS' = newSS { varActivity = refreshActivity
                                     , conflictCount = newConflictCount
                                     }
                      newConflictCount = conflictCount newSS + 1
                      refreshActivity = if newConflictCount `mod` 100 /= 0
                                        then updateActivity (varActivity newSS) learnedClause'
                                        else IntMap.map (* 0.5) $ updateActivity (varActivity newSS) learnedClause'

                      threshold = restartThreshold newSS
                      ifRestart = if newConflictCount == threshold then restart newDB newSS' else newSS'
                      in cdcl newDB ifRestart
        (db', ss', Nothing)  -> case mostActiveVar (varCount db') (assignment ss') (varActivity ss') of -- readers: what unAssigned?
            Nothing      -> SAT (db', ss')
            Just nextVar -> let ss0 = ss' { level = level ss' + 1, assignment = IntMap.insert (getVar nextVar) True $ assignment ss'
                                          , queue = enqueue (varToLit nextVar) $ queue ss'
                                          , trail = trailPush (newLevel $ trail ss') (nextVar, Decided)
                                          }
                            in cdcl db' ss0