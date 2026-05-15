module Engine.Backjump where

import qualified Data.IntMap as IntMap
import qualified Data.Sequence as Seq
import Data.List

import Core.Var
import Core.Lit
import Core.Clause
import Core.Queue
import Core.Trail
import Core.ClauseDB
import Core.SolverState

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
        updatedAssignment = IntMap.insert (getVar firstUIP) theValue asgmtWithoutPoppedVars

        newCID = length cls
        cls = clauses db
        tr = trail ss
        levelInfo = levels tr
        theLiteral = head $ filter (\lit -> litToVar lit == firstUIP) learnedCl
        theValue = litSign theLiteral

        levelChecker :: Level -> Lit -> Level
        levelChecker lv lit = let litLv = levelInfo IntMap.! getVar (litToVar lit) in if litLv > lv && litLv /= currentLevel then litLv else lv