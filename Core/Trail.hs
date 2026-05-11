module Core.Trail where

import qualified Data.IntMap as IntMap

import Core.Var
import Core.Clause

type Level         = Int
type Trail         = [TrailElement]
type TrailElement  = (Var, Bool, Level, Reason)
data Reason        = Decided | Propagated CID deriving (Eq, Ord, Show)

trailPush :: TrailElement -> Trail -> Trail
trailPush trlEle tr = trlEle:tr

trailPop :: Trail -> Trail
trailPop []            = []
trailPop (trlEle:rest) = rest

trailAppend :: Trail -> Var -> Bool -> Level -> Reason -> Trail
trailAppend tr var val lv r = (var, val, lv, r) : tr