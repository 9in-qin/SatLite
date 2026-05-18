module Preprocess where

import qualified Data.Map as Map
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
    (ClauseDB
    { clauses       = dbClauses
    , watchedLits   = IntMap.fromList $ zipWith watching [0..] cls'
    , litsToClauses = IntMap.fromListWith (++) $ concat $ zipWith watchedBy [0..] cls'
    , varCount      = numVars
    },
    SolverState
    { assignment = foldl' assignmentConstructor IntMap.empty unitLits'
    , queue      = enqueueUnitClauses dbClauses Seq.empty
    , trail      = foldl' trailPush emptyTrail varAndRsn
    , varActivity =  IntMap.fromList [(varID, 1.0) | varID <- [0..numVars-1]]
    , conflictCount = 0
    , restartThreshold = 100
    })
    where
        dbClauses = IntMap.fromList $ zipWith listToClause [0..] cls'
        unitLits  = [ (cid, l) | (cid, [l]) <- IntMap.toList dbClauses ]
        unitLits' = map snd unitLits
        unitVars  = map (fmap litToVar) unitLits
        varAndRsn = map (\(x, y) -> (y, Propagated x)) unitVars
        cls'      = nub $ map (map (\x -> x - 2)) $ adjustIndex cls

adjustIndex :: [[Int]] -> [[Int]]
adjustIndex = map step
    where
        step = map intToLit
        intToLit :: Int -> Int
        intToLit i
            | i > 0 = 2 * i
            | otherwise = 2 * (-i) + 1

assignmentConstructor :: IntMap.IntMap Bool -> Lit -> IntMap.IntMap Bool
assignmentConstructor currentAssignment lit = IntMap.insert (getVar $ litToVar lit) (litSign lit) currentAssignment

-- trailConstructor :: Trail -> Lit -> Trail
-- trailConstructor tr lit = (litToVar lit, litSign lit, 0, Propagated 0):tr

listToClause :: CID -> [Int] -> (CID, [Lit])
listToClause cid l = (cid, map Lit l)

-- | Enqueue all unit literals in the BCPqueue for the first round propagation
enqueueUnitClauses :: Clauses -> BCPqueue -> BCPqueue
enqueueUnitClauses cls q = q Seq.>< Seq.fromList [ l | [l] <- IntMap.elems cls ]

watching :: CID -> [Int] -> (CID, (Lit, Lit))
watching cid [x]     = (cid, (Lit x, Lit x))
watching cid (x:y:_) = (cid, (Lit x, Lit y))

watchedBy :: CID -> [Int] -> [(Int, [CID])]
watchedBy cid [x]     = [(x, [cid])]
watchedBy cid (x:y:_) = [(x, [cid]), (y, [cid])]