module Core.WatchedLits where

import qualified Data.IntMap as IntMap

import Core.Lit

type WatchedLits = IntMap.IntMap (Lit, Lit)