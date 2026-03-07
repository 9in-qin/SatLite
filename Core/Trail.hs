module Core.Trail where

import Core.Types

trailPush :: TrailElement -> Trail -> Trail
trailPush trlEle tr = trlEle:tr

trailPop :: Trail -> Trail
trailPop []            = []
trailPop (trlEle:rest) = rest

trailAppend :: Trail -> Var -> Bool -> Level -> Reason -> Trail
trailAppend tr var val lv r = (var, val, lv, r) : tr