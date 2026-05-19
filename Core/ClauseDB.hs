module Core.ClauseDB where

import Core.Clause
import Core.LitsToClauses
import Core.WatchedLits

type VarCount = Int

data ClauseDB = ClauseDB { clauses       :: Clauses
                         , clauseCount   :: Int
                         , watchedLits   :: WatchedLits
                         , litsToClauses :: LitsToClauses
                         , varCount      :: VarCount
                         } deriving (Show)