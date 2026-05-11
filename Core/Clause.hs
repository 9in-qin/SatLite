module Core.Clause where

import qualified Data.IntMap as IntMap

import Core.Lit

type CID           = Int
type Clause        = [Lit]
type Clauses       = IntMap.IntMap Clause