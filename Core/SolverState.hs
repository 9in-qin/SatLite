module Core.SolverState where

import qualified Data.IntMap as IntMap
import qualified Data.Sequence as Seq

import Core.Trail
import Core.Var
import Core.Lit
import Core.Queue
import Core.Assignment
import Decide.VarActivity

data SolverState = SolverState { assignment       :: Assignment
                               , queue            :: BCPqueue
                               , trail            :: Trail
                               , varActivity      :: VarActivity
                               , conflictCount    :: Int
                               , restartThreshold :: Int
                               } deriving (Show)