module Core.Clause where

import qualified Data.Vector as Vector
import qualified Data.IntMap as IntMap

import Core.Lit

type CID            = Int
type Clause         = [Lit]
type FixedClauses   = Vector.Vector Clause
type LearnedClauses = IntMap.IntMap Clause

data Clauses = Clauses { fixedClauses   :: FixedClauses
                       , learnedClauses :: LearnedClauses
                       } deriving Show

emptyClauses :: Clauses
emptyClauses =
    Clauses { fixedClauses   = Vector.empty
            , learnedClauses = IntMap.empty
            }

fixedClausesCount :: Clauses -> Int
fixedClausesCount =
    Vector.length . fixedClauses

learnedClausesCount :: Clauses -> Int
learnedClausesCount =
    IntMap.size . learnedClauses

totalClausesCount :: Clauses -> Int
totalClausesCount cls =
    fixedClausesCount cls + learnedClausesCount cls

insertClause :: CID -> Clause -> Clauses -> Clauses
insertClause cid cl cls =
    cls { learnedClauses = IntMap.insert cid cl (learnedClauses cls) }

lookupClause :: CID -> Clauses -> Clause
lookupClause cid cls
    | cid < fixedClausesCount cls =
        fixedClauses cls Vector.! cid
    | otherwise =
        learnedClauses cls IntMap.! cid

allClauses :: Clauses -> [Clause]
allClauses cls =
    Vector.toList (fixedClauses cls) ++ IntMap.elems (learnedClauses cls)

allClausesWithCID :: Clauses -> [(CID, Clause)]
allClausesWithCID cls =
    zip [0..] $ allClauses cls