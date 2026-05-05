module Core.Types where

import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Data.IntMap as IntMap

newtype Var = Var Int deriving (Eq, Ord, Show)
newtype Lit = Lit Int deriving (Eq, Ord, Show)
newtype Act = Act Int deriving (Eq, Ord, Show)
type VarCount      = Int
type CID           = Int
type Clause        = [Lit]
type Clauses       = IntMap.IntMap Clause
type WatchedLits   = IntMap.IntMap (Lit, Lit)
type LitsToClauses = IntMap.IntMap [CID]
type Assignment    = IntMap.IntMap Bool
data Reason        = Decided | Propagated CID deriving (Eq, Ord, Show)
type Trail         = [TrailElement]
type TrailElement  = (Var, Bool, Level, Reason)
type Level         = Int
type LBD           = Int

type VarActivity   = IntMap.IntMap Double

data Result        = UNSAT | SAT (ClauseDB, SolverState) deriving (Show)
data Result'       = UNSAT' | SAT' Assignment deriving (Show)

data IfConflict    = NoConflict | DoesConflict CID deriving (Show)

data LitType       = LitTrue | LitFalse | LitUnassigned deriving (Show)

type BCPqueue      = Seq.Seq Lit

newtype Val = Val Int deriving (Eq, Ord, Show) --no

data ClauseDB = ClauseDB
    { clauses        :: Clauses
    , learnedIDs     :: [CID]
    , watchedLits    :: WatchedLits
    , litsToClauses  :: LitsToClauses
    , varCount       :: VarCount
    } deriving (Show)

data SolverState = SolverState
    { assignment :: Assignment
    , level      :: Level
    , queue      :: BCPqueue
    , trail      :: Trail
    , varActivity :: VarActivity
    , conflictCount :: Int
    , restartThreshold :: Int
    } deriving (Show)