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

data Result        = UNSAT | SAT (ClauseDB, SolverState) deriving (Show)

cdcl :: ClauseDB -> SolverState -> Result
cdcl db ss =
    let propagation = propagate db ss
    in case hasConflict propagation of
        (db', ss', Just cid) -> case level ss' of -- ss or ss'? as the level has not changed yet (ss' better)
            0  -> UNSAT
            lv -> let (conflictClsToVar, currentLevelVars, currentLevelTrail) = extractInfo (db', ss', cid)
                      (analyzed1, analyzed2) = analyze (conflictClsToVar, currentLevelVars, currentLevelTrail) (clauses db')
                      learnedClause  = learn analyzed1 (assignment ss')
                      (newDB, newSS) = backjump db' ss' learnedClause analyzed2 lv

                      newSS' = newSS {varActivity = refreshActivity,--updateActivity (varActivity newSS) learnedClause,
                                      conflictCount = newConflictCount}
                      newConflictCount = conflictCount newSS + 1
                      refreshActivity = if newConflictCount `mod` 100 /= 0
                                        then updateActivity (varActivity newSS) learnedClause
                                        else IntMap.map (* 0.5) $ updateActivity (varActivity newSS) learnedClause

                      threshold = restartThreshold newSS
                      ifRestart = if newConflictCount == threshold then restart newDB newSS' else newSS'
                      in cdcl newDB ifRestart--{restartThreshold = 2 * restartThreshold ifRestart}--{varActivity = refreshActivity}
                  --in cdcl newDB newSS'
        --(db', ss', Nothing)  -> case unAssigned (varCount db') (assignment ss') of -- readers: what unAssigned?
        (db', ss', Nothing)  -> case mostActiveVar (varCount db') (assignment ss') (varActivity ss') of -- readers: what unAssigned?
            Nothing      -> SAT (db', ss')
            Just nextVar -> let ss0 = ss' {level = level ss' + 1, assignment = IntMap.insert (getVar nextVar) True $ assignment ss',
                                          queue = enqueue (varToLit nextVar) $ queue ss',
                                          trail = trailAppend (trail ss') nextVar True (level ss' + 1) Decided}
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

extractInfo :: (ClauseDB, SolverState, CID) -> ([Var], [Var], [TrailElement])
extractInfo (db, ss, cid) = let conflictClsToVar  = map litToVar $ clauses db IntMap.! cid
                                currentLevelTrail = [(var, val, lv, r) | (var, val, lv, r) <- trail ss, lv == currentLevel]
                                currentLevelVars  = map (\(var, _, _, _) -> var) currentLevelTrail
                            in (conflictClsToVar, currentLevelVars, currentLevelTrail)
                            where
                                currentLevel = level ss

analyze :: ([Var], [Var], [TrailElement]) -> Clauses -> ([Var], Var)
analyze (vars, currentLevelVars, tr) cls
    | exactlyOne currentLevelVarsLeft = (vars, head currentLevelVarsLeft)
    | otherwise = analyze (newVars, currentLevelVars, tr) cls
        where
            currentLevelVarsLeft = vars `intersect` currentLevelVars
            newVars = nub $ concatMap replaceVar vars

            exactlyOne [x] = True
            exactlyOne _   = False
            
            replaceVar :: Var -> [Var]
            replaceVar var
                | var `elem` currentLevelVars =
                    case [r | (v, _, _, r) <- tr, v == var] of
                        [Decided]        -> [var]
                        [Propagated cid] -> filter (/= var) $ map litToVar $ cls IntMap.! cid
                | otherwise = [var]

learn :: [Var] -> Assignment -> [Lit]
learn vars asgmt = map step vars
    where
        step :: Var -> Lit
        step var
            | asgmt IntMap.! getVar var = negateLit $ varToLit var -- learn the opposite
            | otherwise       = varToLit var

backjump :: ClauseDB -> SolverState -> Clause -> Var -> Level -> (ClauseDB, SolverState)
backjump db ss learnedCl theVar currentLevel =
    (db{clauses = updatedClauses},
     ss{assignment = updatedAssignment, level = backjumpLevel, queue = updatedQueue, trail = updatedTrail})
    where
        updatedClauses = IntMap.insert newCID learnedCl cls
        backjumpLevel = if length learnedCl == 1 then 0
                        else maximum [level | (var, _, level, _) <- tr, var `elem` map litToVar learnedCl, level < currentLevel]
        updatedQueue = enqueue theLiteral Seq.empty
        updatedTrail = (theVar, theValue, backjumpLevel, Propagated newCID) : filter (\(_, _, lv, _) -> lv <= backjumpLevel) tr
        updatedAssignment = IntMap.insert (getVar theVar) theValue (IntMap.restrictKeys (assignment ss) (IntSet.fromList keptVars))
        newCID = length cls
        cls = clauses db
        tr = trail ss
        theLiteral = head $ filter (\lit -> litToVar lit == theVar) learnedCl
        theValue = litSign theLiteral
        keptVars = map (\(var, _, _, _) -> getVar var) updatedTrail

--VSIDS related functions
updateActivity :: VarActivity -> Clause -> VarActivity
updateActivity = --foldl' updateCertainVar
    foldl' (\ac (Lit i) -> IntMap.insertWith (+) (getVar $ litToVar (Lit i)) 1.0 ac)
    -- where
    --     updateCertainVar :: Map.Map Var Double -> Lit -> Map.Map Var Double
    --     updateCertainVar ac (Lit i) = Map.insertWith (+) (Var i) 1.0 ac