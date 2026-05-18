module Engine.Propagate where

import qualified Data.Sequence as Seq
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet
import Data.List

import Core.Var
import Core.Lit
import Core.Trail
import Core.Clause
import Core.ClauseDB
import Core.SolverState
import Core.Queue
import Core.Assignment

data IfConflict = NoConflict | DoesConflict CID deriving (Show)

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
            let cl = clauses db0 IntMap.! cid
            in checkClause lit cid cl db0 ss0

checkClause :: Lit -> CID -> Clause -> ClauseDB -> SolverState -> (ClauseDB, SolverState, IfConflict)
checkClause lit cid cl db ss =
    case IntMap.lookup otherWatchKey asgmt of
        Just otherWatchValue ->
            if ifLiteralTrue theOtherWatch otherWatchValue
                then (db, ss, NoConflict)
                else maybe (db, ss, DoesConflict cid) helper (findNewWatch asgmt cl lit theOtherWatch)
        Nothing ->
            case findNewWatch asgmt cl lit theOtherWatch of
                    Nothing   -> let q'               = enqueue theOtherWatch q
                                     otherWatchNewVal = litSign theOtherWatch
                                     assignment'      = IntMap.insert otherWatchKey otherWatchNewVal (assignment ss)
                                     trail'           = trailPush (trail ss) (otherWatchVar, Propagated cid)
                                 in (db, ss { assignment = assignment'
                                            , queue      = q'
                                            , trail      = trail'
                                            }, NoConflict)
                    Just lit0 -> helper lit0
    where
        dbWatchedLits = watchedLits db
        (w0, w1)      = dbWatchedLits IntMap.! cid
        theOtherWatch = if lit /= w0 then w0 else w1
        otherWatchVar = litToVar theOtherWatch
        otherWatchKey = getVar otherWatchVar
        asgmt         = assignment ss
        q             = queue ss
        litsToCls     = litsToClauses db

        helper newWatch = let updatedWatchedLits = IntMap.insert cid (Lit newWatch, theOtherWatch) dbWatchedLits
                              updatedLitsToClauses =
                                 case IntMap.lookup newWatch litsToCls of
                                     Just newWatchedBy -> IntMap.insert newWatch (cid:newWatchedBy) litsToCls
                                     Nothing           -> IntMap.insert newWatch [cid] litsToCls
                              -- the following line update the original "lit" by removing it, which is a watched literal before
                              updatedLitsToClauses' = IntMap.insert (getLit lit) [cid' | cid' <- litsToCls IntMap.! getLit lit, cid' /= cid] updatedLitsToClauses
                          in (db { watchedLits = updatedWatchedLits, litsToClauses = updatedLitsToClauses'}, ss, NoConflict)

findNewWatch :: Assignment -> Clause -> Lit -> Lit -> Maybe Int
findNewWatch asgmt [_, _] oldWatch otherWatch = Nothing
findNewWatch asgmt cl oldWatch otherWatch =
    step cl
    where
        step [] = Nothing
        step (l:ls)
            | l == oldWatch || l == otherWatch = step ls
            | otherwise =
                case literalType asgmt l of
                    LitFalse -> step ls
                    _        -> Just (getLit l)

-- findNewWatch asgmt [_] lit0 lit1 = Nothing -- should never happen, cause unit clause should be automatically true
-- findNewWatch asgmt cl lit0 lit1 =
--     case foldl' trueOrUnassigned (Nothing, LitFalse) newCl of
--         (_, LitFalse)  -> Nothing
--         (Just resultLit, _) -> Just (getLit resultLit)
--     where
--         newCl = [lit | lit <- cl, lit /= lit0 && lit /= lit1]

--         trueOrUnassigned :: (Maybe Lit, LitType) -> Lit -> (Maybe Lit, LitType)
--         trueOrUnassigned (potentialLit, LitTrue) _   = (potentialLit, LitTrue)
--         trueOrUnassigned (potentialLit, litType) lit =
--             case literalType asgmt lit of
--                 LitFalse -> (potentialLit, litType)
--                 tOrU     -> (Just lit, tOrU)