module Core.SolverState where

import Core.Assignment
import Core.Queue
import Core.Trail
import Decide.VarActivity

data SolverState = SolverState { assignment       :: Assignment
                               , queue            :: BCPqueue
                               , trail            :: Trail
                               , varActivity      :: VarActivity
                               , conflictCount    :: Int
                               , restartThreshold :: Int
                               } deriving (Show)