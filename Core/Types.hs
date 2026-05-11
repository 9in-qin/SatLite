module Core.Types where

-- import qualified Data.Map as Map
-- import qualified Data.Sequence as Seq
-- import qualified Data.IntMap as IntMap

-- import Core.Var
-- import Core.Lit
-- import Core.Trail
-- import Core.Clause
-- import Core.ClauseDB
-- import Core.SolverState
-- import Core.Queue

--newtype Var = Var Int deriving (Eq, Ord, Show)
--newtype Lit = Lit Int deriving (Eq, Ord, Show)
-- type VarCount      = Int
-- type CID           = Int
-- type Clause        = [Lit]
-- type Clauses       = IntMap.IntMap Clause
-- type WatchedLits   = IntMap.IntMap (Lit, Lit)
-- type LitsToClauses = IntMap.IntMap [CID]
-- type Assignment    = IntMap.IntMap Bool
-- type Trail         = [TrailElement]
-- type TrailElement  = (Var, Bool, Level, Reason)
-- data Reason        = Decided | Propagated CID deriving (Eq, Ord, Show)
-- type Level         = Int
--type LBD           = Int

-- type VarActivity   = IntMap.IntMap Double

-- data Result        = UNSAT | SAT (ClauseDB, SolverState) deriving (Show)
--data Result'       = UNSAT' | SAT' Assignment deriving (Show)

-- data IfConflict    = NoConflict | DoesConflict CID deriving (Show)

-- data LitType       = LitTrue | LitFalse | LitUnassigned deriving (Show)

-- type BCPqueue      = Seq.Seq Lit

-- data ClauseDB = ClauseDB
--     { clauses        :: Clauses
--     , learnedIDs     :: [CID]
--     , watchedLits    :: WatchedLits
--     , litsToClauses  :: LitsToClauses
--     , varCount       :: VarCount
--     } deriving (Show)

-- data SolverState = SolverState
--     { assignment :: Assignment
--     , level      :: Level
--     , queue      :: BCPqueue
--     , trail      :: Trail
--     , varActivity :: VarActivity
--     , conflictCount :: Int
--     , restartThreshold :: Int
--     } deriving (Show)