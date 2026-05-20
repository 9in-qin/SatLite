module Preprocess where

import qualified Data.IntMap as IntMap
import qualified Data.Sequence as Seq
import Data.List

import Core.Var
import Core.Lit
import Core.Trail
import Core.Clause
import Core.ClauseDB
import Core.SolverState
import Core.Queue

preprocess :: ([[Int]], (Int, Int)) -> (ClauseDB, SolverState)
preprocess (cls, (numVars, numClauses)) =
    (ClauseDB { clauses       = dbClauses
              , clauseCount   = length dbClauses
              , watchedLits   = IntMap.fromList $ zipWith watching [0..] cls'
              , litsToClauses = IntMap.fromListWith (++) $ concat $ zipWith watchedBy [0..] cls'
              , varCount      = numVars
              },
    SolverState { assignment       = foldl' assignmentConstructor IntMap.empty unitLits
                , queue            = enqueueUnitClauses unitLits Seq.empty
                , trail            = foldl' trailPush emptyTrail varAndRsn
                , varActivity      = IntMap.fromList [(varID, 1.0) | varID <- [0..numVars-1]]
                , conflictCount    = 0
                , restartThreshold = 100
                })
    where
        cls'      = nub $ map (map (\x -> x - 2)) $ adjustIndex cls
        dbClauses = IntMap.fromList $ zipWith listToClause [0..] cls'
        unitCls   = [ (cid, l) | (cid, [l]) <- IntMap.toList dbClauses ]
        unitLits  = map snd unitCls
        unitVars  = map (fmap litToVar) unitCls
        varAndRsn = map (\(x, y) -> (y, Propagated x)) unitVars

adjustIndex :: [[Int]] -> [[Int]]
adjustIndex = map $ map intToLitIndex
    where
        intToLitIndex :: Int -> Int
        intToLitIndex i
            | i > 0 = 2 * i
            | otherwise = 2 * (- i) + 1

assignmentConstructor :: IntMap.IntMap Bool -> Lit -> IntMap.IntMap Bool
assignmentConstructor currentAssignment lit = IntMap.insert (getVar $ litToVar lit) (litSign lit) currentAssignment

listToClause :: CID -> [Int] -> (CID, [Lit])
listToClause cid l = (cid, map Lit l)

enqueueUnitClauses :: [Lit] -> BCPqueue -> BCPqueue
enqueueUnitClauses lits q = q Seq.>< Seq.fromList lits

watching :: CID -> [Int] -> (CID, (Lit, Lit))
watching cid [x]     = (cid, (Lit x, Lit x))
watching cid (x:y:_) = (cid, (Lit x, Lit y))

watchedBy :: CID -> [Int] -> [(Int, [CID])]
watchedBy cid [x]     = [(x, [cid])]
watchedBy cid (x:y:_) = [(x, [cid]), (y, [cid])]