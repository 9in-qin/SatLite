module Preprocess where

import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import Data.List

import Core.Types
import Core.VarLit (litSign, litToVar)

preprocess :: ([[Int]], (Int, Int)) -> (ClauseDB, SolverState)
preprocess (cls, (numVars, numClauses)) =
    (ClauseDB
    { clauses       = dbClauses
    , learnedIDs    = []
    , watchedLits   = Map.fromList $ zipWith watching [0..] cls'
    , litsToClauses = Map.fromListWith (++) $ concat $ zipWith watchedBy [0..] cls'
    , varCount      = numVars
    },
    SolverState
    { assignment = foldl' assignmentConstructor Map.empty unitCls
    , level      = 0
    , queue      = enqueueUnitClauses dbClauses Seq.empty
    , trail      = foldl' trailConstructor [] unitCls
    , varActivity =  Map.fromList [(Var varID, 1.0) | varID <- [0..numVars-1]]
    , conflictCount = 0
    --, restartCount = 0
    , restartThreshold = 100
    })
    where
        dbClauses = Map.fromList $ zipWith listToClause [0..] cls'
        unitCls   = [ l | [l] <- Map.elems dbClauses ]
        cls'      = map nub cls

assignmentConstructor :: Map.Map Var Bool -> Lit -> Map.Map Var Bool
assignmentConstructor currentAssignment lit = Map.insert (litToVar lit) (litSign lit) currentAssignment

trailConstructor :: Trail -> Lit -> Trail
trailConstructor tr lit = (litToVar lit, litSign lit, 0, Propagated 0):tr

listToClause :: CID -> [Int] -> (CID, [Lit])
listToClause cid l = (cid, map Lit l)

-- | Enqueue all unit literals in the BCPqueue for the first round propagation
enqueueUnitClauses :: Clauses -> BCPqueue -> BCPqueue
enqueueUnitClauses cls q = q Seq.>< Seq.fromList [ l | [l] <- Map.elems cls ]

watching :: CID -> [Int] -> (CID, (Lit, Lit))
watching cid [x]     = (cid, (Lit x, Lit x))
watching cid (x:y:_) = (cid, (Lit x, Lit y))

watchedBy :: CID -> [Int] -> [(Lit, [CID])]
watchedBy cid [x]     = [(Lit x, [cid])]
watchedBy cid (x:y:_) = [(Lit x, [cid]), (Lit y, [cid])]