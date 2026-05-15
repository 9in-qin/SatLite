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

propagate :: ClauseDB -> SolverState -> (ClauseDB, SolverState, IfConflict)
propagate db ss =
    case Seq.viewl $ queue ss of -- may need to replace with helper function for better modularity (later)
        Seq.EmptyL          -> (db, ss, NoConflict) -- if there is nothing to be propagated in the queue, do nothing
        (Lit i) Seq.:< rest ->
            case processWatched (negateLit (Lit i)) db ss of -- negation lit is better, revise later
                (db', ss', DoesConflict cid ) -> (db', ss', DoesConflict cid)
                (db', ss', NoConflict)        -> propagate db' ss'

hasConflict :: (ClauseDB, SolverState, IfConflict) -> (ClauseDB, SolverState, Maybe CID)
hasConflict (db, ss, NoConflict) = (db, ss, Nothing)
hasConflict (db, ss, DoesConflict cid) = (db, ss, Just cid)

extractInfo :: (ClauseDB, SolverState, CID) -> (ResolutionClause, TrailElements, Reasons)
extractInfo (db, ss, cid) =
    (conflictClsToVar, trailEles, reasons tr)
    where
        conflictClsToVar = IntMap.fromList $ map (\lit -> (getVar $ litToVar lit, lit)) (clauses db IntMap.! cid)
        trailEles = currentLevelTrail tr
        tr = trail ss

backjump :: ClauseDB -> SolverState -> Clause -> Var -> Level -> (ClauseDB, SolverState)
backjump db ss learnedCl firstUIP currentLevel =
    (db{clauses = updatedClauses},
     ss{ assignment = updatedAssignment
       , level = backjumpLevel
       , queue = updatedQueue
       , trail = updatedTrail
       })
    where
        updatedClauses = IntMap.insert newCID learnedCl cls
        backjumpLevel = if length learnedCl == 1 then 0
                        else foldl' levelChecker 0 learnedCl -- start with 0?
        updatedQueue = enqueue theLiteral Seq.empty

        (trailAfterPop, poppedVars) = trailPopToLevel tr backjumpLevel
        updatedTrail = trailPush trailAfterPop (firstUIP, Propagated newCID)
        
        asgmt = assignment ss
        asgmtWithoutPoppedVars = foldl' (\acc x -> IntMap.delete (getVar x) acc) asgmt poppedVars
        updatedAssignment = IntMap.insert (getVar firstUIP) theValue asgmtWithoutPoppedVars--(IntMap.restrictKeys (assignment ss) (IntSet.fromList keptVars))

        newCID = length cls
        cls = clauses db
        tr = trail ss
        levelInfo = levels tr
        theLiteral = head $ filter (\lit -> litToVar lit == firstUIP) learnedCl
        theValue = litSign theLiteral
        --keptVars = map getVar unpoppedVars

        levelChecker :: Level -> Lit -> Level
        levelChecker lv lit = let litLv = levelInfo IntMap.! getVar (litToVar lit) in if litLv > lv && litLv /= currentLevel then litLv else lv