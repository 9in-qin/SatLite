module Engine.Analyze where

import qualified Data.Map as Map
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Sequence as Seq
import Data.List

import Core.Var
import Core.Lit
import Core.Clause
import Core.Trail

type ResolutionClause = IntMap.IntMap Lit
type CurrentLevelVars = [Var]

analyze :: (ResolutionClause, TrailElements, Reasons) -> CurrentLevelVars -> Clauses -> (ResolutionClause, Var)
analyze (resCl, trEles, rsns) currLvVars cls =
    case exactlyOne resCl currLvVars of
        Just firstUIP ->
            (resCl, firstUIP)
        Nothing       ->
            case IntMap.lookup (getVar workingVar) resCl of
                Just lit ->
                    case rsns IntMap.! getVar workingVar of
                        Propagated cid ->
                            let
                                resolvedCl = resolution resCl workingVar (cls IntMap.! cid)
                            in
                                analyze (resolvedCl, remainingTrail, rsns) currLvVars cls
                        Decided        -> error "Should never reach this stage."
                Nothing  ->
                    analyze (resCl, remainingTrail, rsns) currLvVars cls
    where
        workingVar     = head trEles
        remainingTrail = tail trEles

exactlyOne :: ResolutionClause -> CurrentLevelVars -> Maybe Var
exactlyOne resCl currLvVars =
    case filter inResCl currLvVars of
        [x] -> Just x
        _   -> Nothing
    where
        inResCl var = IntMap.member (getVar var) resCl

resolution :: ResolutionClause -> Var -> Clause -> ResolutionClause
resolution resCl var cl = foldl' insertLit resClWithoutVar clWithoutVar
    where
        resClWithoutVar = IntMap.delete (getVar var) resCl
        clWithoutVar    = filter (\lit -> litToVar lit /= var) cl

        insertLit :: ResolutionClause -> Lit -> ResolutionClause
        insertLit resCl' lit' = IntMap.insert (getVar $ litToVar lit') lit' resCl'