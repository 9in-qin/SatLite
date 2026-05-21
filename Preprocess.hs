module Preprocess where

import qualified Data.Vector as Vector
import qualified Data.IntMap as IntMap
import Data.List

import Core.Assignment
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
              , watchedLits   = IntMap.fromList $ zipWith watching [0..] formalCls
              , litsToClauses = IntMap.fromListWith (++) $ concat $ zipWith watchedBy [0..] formalCls
              , varCount      = numVars
              },
    SolverState { assignment       = foldl' assignLiteral emptyAssignment unitLits
                , queue            = enqueueUnitClauses unitLits emptyQueue
                , trail            = foldl' trailPush emptyTrail varAndRsn
                , varActivity      = IntMap.fromList [(varID, 1.0) | varID <- [0..numVars-1]]
                , conflictCount    = 0
                , restartThreshold = 100
                })
    where
        formalCls = map (map (Lit . intToLitIndex)) cls
        dbClauses = Clauses { fixedClauses = Vector.fromList formalCls
                            , learnedClauses = IntMap.empty
                            }
        unitCls   = [ (cid, l) | (cid, [l]) <- IntMap.toList dbClauses ]
        unitLits  = map snd unitCls
        unitVars  = map (fmap litToVar) unitCls
        varAndRsn = map (\(x, y) -> (y, Propagated x)) unitVars

intToLitIndex :: Int -> Int
intToLitIndex i
    | i > 0 = 2 * (i - 1)
    | otherwise = 2 * (- i) - 1

watching :: CID -> [Int] -> (CID, (Lit, Lit))
watching cid [l]       = (cid, (Lit l, Lit l))
watching cid (l0:l1:_) = (cid, (Lit l0, Lit l1))

watchedBy :: CID -> [Int] -> [(Int, [CID])]
watchedBy cid [l]       = [(l, [cid])]
watchedBy cid (l0:l1:_) = [(l0, [cid]), (l1, [cid])]