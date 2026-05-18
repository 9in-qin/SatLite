module Engine.CDCL where

import qualified Data.IntMap as IntMap
import qualified Data.Sequence as Seq

import Core.Var
import Core.Lit
import Core.Clause
import Core.Queue
import Core.Trail
import Core.Watched
import Core.Restart
import Core.ClauseDB
import Core.SolverState
import Decide.VSIDS
import Decide.VarActivity
import Engine.Analyze
import Engine.Backjump

data Result = UNSAT | SAT (ClauseDB, SolverState) deriving (Show)

cdcl :: ClauseDB -> SolverState -> Result
cdcl db ss =
    let propagation = propagate db ss
    in case propagation of
        (db', ss', DoesConflict cid) -> case currentLevel $ trail ss' of
            0  -> UNSAT
            lv -> let (newDB, newSS) = processConflict lv cid db' ss'
                  in cdcl newDB newSS
        (db', ss', NoConflict) -> case mostActiveVar (varCount db') (assignment ss') (varActivity ss') of
            Nothing      -> SAT (db', ss')
            Just nextVar -> let newSS = processDecide ss' nextVar
                            in cdcl db' newSS

processConflict :: Level -> CID -> ClauseDB -> SolverState -> (ClauseDB, SolverState)
processConflict lv cid db ss = (newDB, newSS)
    where
        (learned, firstUIP) = analyze (extractInfo (db, ss, cid)) (clauses db)
        learnedClause       = IntMap.elems learned
        (newDB, backjumpSS) = backjump db ss learnedClause firstUIP lv
        newConflictCount    = conflictCount backjumpSS + 1
        newVarActivity      = conflictBasedUpdate newConflictCount (varActivity backjumpSS) learnedClause
        updatedSS = backjumpSS { varActivity = newVarActivity
                               , conflictCount = newConflictCount
                               }
                               
        newSS = if newConflictCount == restartThreshold updatedSS
                then restart newDB updatedSS
                else updatedSS

processDecide :: SolverState -> Var -> SolverState
processDecide ss nextVar =
    ss { assignment = IntMap.insert (getVar nextVar) True $ assignment ss
       , queue = enqueue (varToLit nextVar) $ queue ss
       , trail = trailPush (newLevel $ trail ss) (nextVar, Decided)
       }