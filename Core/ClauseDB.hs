module Core.ClauseDB where

import qualified Data.IntMap as IntMap

import Core.Lit
import Core.Clause
import Core.WatchedLits
import Core.LitsToClauses

type VarCount = Int

data ClauseDB = ClauseDB { clauses       :: Clauses
                         , watchedLits   :: WatchedLits
                         , litsToClauses :: LitsToClauses
                         , varCount      :: VarCount
                         } deriving (Show)