module Engine.Analyze where

import qualified Data.IntMap as IntMap
import Data.List

import Core.Clause
import Core.ClauseDB
import Core.Lit
import Core.SolverState
import Core.Trail
import Core.Var

type ResolutionClause = IntMap.IntMap Lit
type TrailElements    = [Var]
type CurrentLevelVars = [Var]

extractInfo :: (ClauseDB, SolverState, CID) -> (ResolutionClause, TrailElements, Reasons)
extractInfo (db, ss, cid) =
    (resCl, trailEles, reasons tr)
    where
        conflictCl = lookupClause cid (clauses db)
        resCl      = IntMap.fromList $ map (\lit -> (getVar (litToVar lit), lit)) conflictCl
        trailEles  = currentLevelTrail tr
        tr         = trail ss

analyze :: (ResolutionClause, TrailElements, Reasons) -> Clauses -> (ResolutionClause, Var)
analyze (resCl, trEles, rsns) cls =
    case exactlyOne resCl trEles of
        Just firstUIP -> (resCl, firstUIP)
        Nothing       ->
            case IntMap.lookup (getVar workingVar) resCl of
                Just lit ->
                    case rsns IntMap.! getVar workingVar of
                        Propagated cid ->
                            let resolvedCl = resolution resCl workingVar (lookupClause cid cls)
                            in analyze (resolvedCl, remainingTrail, rsns) cls
                        Decided -> error "Should never reach this stage."
                Nothing -> analyze (resCl, remainingTrail, rsns) cls
    where
        workingVar     = head trEles
        remainingTrail = tail trEles

exactlyOne :: ResolutionClause -> CurrentLevelVars -> Maybe Var
exactlyOne resCl = find Nothing
    where
        find acc [] = acc
        find Nothing (var:vars) =
            if IntMap.member (getVar var) resCl
            then find (Just var) vars
            else find Nothing vars
        find (Just x) (var:vars) =
            if IntMap.member (getVar var) resCl
            then Nothing
            else find (Just x) vars

resolution :: ResolutionClause -> Var -> Clause -> ResolutionClause
resolution resCl var cl =
    foldl' insertLit resClWithoutVar clWithoutVar
    where
        resClWithoutVar = IntMap.delete (getVar var) resCl
        clWithoutVar    = filter (\lit -> litToVar lit /= var) cl

        insertLit resCl' lit' =
            IntMap.insert (getVar (litToVar lit')) lit' resCl'