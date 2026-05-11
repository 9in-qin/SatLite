module Core.LitsToClauses where

import qualified Data.IntMap as IntMap

import Core.Clause

type LitsToClauses = IntMap.IntMap [CID]