module Engine.Propagate where

import qualified Data.Sequence as Seq
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet
import Data.List

import Core.Assignment
import Core.Clause
import Core.ClauseDB
import Core.Lit
import Core.LitsToClauses
import Core.Queue
import Core.SolverState
import Core.Trail
import Core.Var

data IfConflict = NoConflict | DoesConflict CID

propagate :: ClauseDB -> SolverState -> (ClauseDB, SolverState, IfConflict)
propagate db ss =
    case Seq.viewl (queue ss) of
        Seq.EmptyL      -> (db, ss, NoConflict)
        lit Seq.:< rest ->
            case processWatched (negateLit lit) db ss { queue = rest } of
                (db', ss', DoesConflict cid ) -> (db', ss', DoesConflict cid)
                (db', ss', NoConflict)        -> propagate db' ss'

processWatched :: Lit -> ClauseDB -> SolverState -> (ClauseDB, SolverState, IfConflict)
processWatched lit db ss =
    case IntMap.lookup (getLit lit) (litsToClauses db) of
        Nothing   -> (db, ss, NoConflict)
        Just cids -> processInfluenced lit cids db ss

processInfluenced :: Lit -> [CID] -> ClauseDB -> SolverState -> (ClauseDB, SolverState, IfConflict)
processInfluenced lit cids db ss =
    foldl' step (db, ss, NoConflict) cids
    where
        step acc@(_, _, DoesConflict _) _ = acc
        step (db0, ss0, NoConflict) cid =
            let cl = lookupClause cid (clauses db0)
            in checkClause lit cid cl db0 ss0

checkClause :: Lit -> CID -> Clause -> ClauseDB -> SolverState -> (ClauseDB, SolverState, IfConflict)
checkClause lit cid cl db ss =
    case IntMap.lookup otherWatchKey asgmt of
        Just otherWatchValue ->
            if ifLiteralTrue theOtherWatch otherWatchValue
            then (db, ss, NoConflict)
            else maybe (db, ss, DoesConflict cid) processWatchUpdate ptnlNewWatch
        Nothing ->
            maybe unitPropagateOtherWatch processWatchUpdate ptnlNewWatch
    where
        dbWatchedLits = watchedLits db
        (w0, w1)      = dbWatchedLits IntMap.! cid
        theOtherWatch = if lit /= w0 then w0 else w1
        otherWatchVar = litToVar theOtherWatch
        otherWatchKey = getVar otherWatchVar
        ptnlNewWatch  = findNewWatch asgmt cl lit theOtherWatch
        asgmt         = assignment ss
        q             = queue ss
        litsToCls     = litsToClauses db

        processWatchUpdate (Lit newWatch) =
            let lsToClsRemoveOldWatch = IntMap.adjust (delete cid) (getLit lit) litsToCls
            in (db { watchedLits   = IntMap.insert cid (Lit newWatch, theOtherWatch) dbWatchedLits
                   , litsToClauses = updateLitsToClauses lsToClsRemoveOldWatch newWatch cid
                   }, ss, NoConflict)

        unitPropagateOtherWatch =
            let otherWatchNewVal = litSign theOtherWatch
            in (db, ss { assignment = IntMap.insert otherWatchKey otherWatchNewVal (assignment ss)
                       , queue      = enqueue theOtherWatch q
                       , trail      = trailPush (trail ss) (otherWatchVar, Propagated cid)
                       }, NoConflict)

findNewWatch :: Assignment -> Clause -> Lit -> Lit -> Maybe Lit
findNewWatch asgmt [_, _] oldWatch otherWatch = Nothing
findNewWatch asgmt cl oldWatch otherWatch =
    step cl
    where
        step [] = Nothing
        step (lit:lits)
            | lit == oldWatch || lit == otherWatch =
                step lits
            | otherwise =
                case literalType asgmt lit of
                    LitFalse -> step lits
                    _        -> Just lit