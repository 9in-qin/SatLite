module Core.Watched where

import qualified Data.Map as Map
import qualified Data.Set as Set

import Core.Types
import Core.VarLit
import Core.Queue
import Core.Trail
import Data.List (foldl')

processWatched :: Lit -> ClauseDB -> SolverState -> (ClauseDB, SolverState, IfConflict)
processWatched lit db ss =
    case Map.lookup lit litsToCls of
        Nothing    -> (db, ss {queue = dequeue $ queue ss}, NoConflict) --dequeue is important, or cdcl will be an infinite loop
        Just cids  ->
            let influencedCls = influencedClauses cids cls -- foldWithKey' omit toList
            in case processInfluenced lit influencedCls db ss of
                (lit0, db0, ss0, DoesConflict cid) -> (db0, ss0 {queue = dequeue $ queue ss0}, DoesConflict cid)
                (lit0, db0, ss0, NoConflict)       -> (db0, ss0 {queue = dequeue $ queue ss0}, NoConflict)
    where
        cls       = clauses db
        litsToCls = litsToClauses db

influencedClauses :: [CID] -> Clauses -> Clauses
influencedClauses cids cls = Map.restrictKeys cls (Set.fromList cids) -- restrictKeys is more efficient

processInfluenced :: Lit -> Clauses -> ClauseDB -> SolverState -> (Lit, ClauseDB, SolverState, IfConflict)
processInfluenced lit cls db ss =
    Map.foldlWithKey' step (lit, db, ss, NoConflict) cls
    where
        step (lit0, db0, ss0, DoesConflict cid) _ _ = (lit0, db0, ss0, DoesConflict cid)
        step (lit0, db0, ss0, NoConflict) cid cl    =
            case checkClause lit0 cid cl db0 ss0 of
                (lit0, db1, ss1, NoConflict)        -> (lit0, db1, ss1, NoConflict)
                (lit0, db1, ss1, DoesConflict cid1) -> (lit0, db1, ss1, DoesConflict cid1)

checkClause :: Lit -> CID -> Clause -> ClauseDB -> SolverState -> (Lit, ClauseDB, SolverState, IfConflict)
checkClause lit cid cl db ss =
    case Map.lookup theOtherWatchVar asgmt of
        Just otherWatchValue ->
            if ifLiteralTrue theOtherWatch otherWatchValue
                then (lit, db, ss, NoConflict) -- if the other watch is already true, leave this clause alone
                else maybe (lit, db, ss, DoesConflict cid) helper (findNewWatch asgmt cl lit theOtherWatch)
        Nothing -> --(lit, db, ss, NoConflict) -- propagation happens here, update everything -----not really here
            case findNewWatch asgmt cl lit theOtherWatch of
                    Nothing   -> let q'               = enqueue theOtherWatch q
                                     otherWatchNewVal = litSign theOtherWatch
                                     assignment'      = Map.insert theOtherWatchVar otherWatchNewVal (assignment ss)
                                     trail'           = trailAppend (trail ss) theOtherWatchVar otherWatchNewVal l (Propagated cid)
                                 in (lit, db, ss {assignment = assignment', queue = q', trail = trail'}, NoConflict)
                    Just lit0 -> helper lit0
    where
        cls       = clauses db
        dbWatchedLits = watchedLits db
        watched   = dbWatchedLits Map.! cid
        theOtherWatch = let first = fst watched in if first == lit then snd watched else first
        theOtherWatchVar = litToVar theOtherWatch
        litsToCls = litsToClauses db
        asgmt     = assignment ss
        q         = queue ss
        l         = level ss

        helper newWatch = let updatedWatchedLits = Map.insert cid (newWatch, theOtherWatch) dbWatchedLits
                              updatedLitsToClauses =
                                 case Map.lookup newWatch litsToCls of
                                     Just newWatchedBy -> Map.insert newWatch (cid:newWatchedBy) litsToCls
                                     Nothing           -> Map.insert newWatch [cid] litsToCls
                              -- the following line update the original "lit" by removing it, which is a watched literal before
                              updatedLitsToClauses' = Map.insert lit [cid' | cid' <- litsToCls Map.! lit, cid' /= cid] updatedLitsToClauses
                          in (lit, db { watchedLits = updatedWatchedLits, litsToClauses = updatedLitsToClauses'}, ss, NoConflict)

findNewWatch :: Assignment -> Clause -> Lit -> Lit -> Maybe Lit
findNewWatch asgmt [_] lit0 lit1   = Nothing -- should be checked ealier instead of in this function, revise later
findNewWatch asgmt [_,_] lit0 lit1 = Nothing
findNewWatch asgmt cl lit0 lit1    =
    case foldl' trueOrUnassigned (Nothing, LitFalse) newCl of
        (_, LitFalse)  -> Nothing
        (resultLit, _) -> resultLit
    where
        newCl = [lit | lit <- cl, lit /= lit0 && lit /= lit1]

        trueOrUnassigned :: (Maybe Lit, LitType) -> Lit -> (Maybe Lit, LitType)
        trueOrUnassigned (potentialLit, LitTrue) _   = (potentialLit, LitTrue)
        trueOrUnassigned (potentialLit, litType) lit =
            case literalType asgmt lit of
                LitFalse -> (potentialLit, litType)
                tOrU     -> (Just lit, tOrU)