module Core.Trail where

import qualified Data.IntMap as IntMap
import Data.Maybe
import Data.List

import Core.Var
import Core.Clause

type Level        = Int
type LvBasedTrail = [Var]
data Reason       = Decided | Propagated CID deriving (Eq, Ord, Show)
type Reasons      = IntMap.IntMap Reason
type Levels       = IntMap.IntMap Level

data Trail = Trail { currentLevel  :: !Level
                   , lvBasedTrails :: ![LvBasedTrail]
                   , reasons       :: !Reasons
                   , levels        :: !Levels
                   } deriving (Show)

emptyTrail :: Trail
emptyTrail =
    Trail { currentLevel  = 0
    , lvBasedTrails = [[]]
    , reasons       = IntMap.empty
    , levels        = IntMap.empty
    }

currentLevelTrail :: Trail -> LvBasedTrail
currentLevelTrail tr =
    currentLvTr
    where currentLvTr : _ = lvBasedTrails tr

newLevel :: Trail -> Trail
newLevel tr =
    tr { currentLevel  = currentLevel tr + 1
       , lvBasedTrails = [] : lvBasedTrails tr
       }

trailPush :: Trail -> (Var, Reason) -> Trail
trailPush tr (var, rsn) =
    tr { lvBasedTrails = updatedLvBasedTrails
       , reasons       = IntMap.insert varKey rsn (reasons tr)
       , levels        = IntMap.insert varKey (currentLevel tr) (levels tr)
       }
    where
        varKey                       = getVar var
        currentTrail : lowerLvTrails = lvBasedTrails tr
        updatedLvBasedTrails         = (var : currentTrail) : lowerLvTrails

trailPopToLevel :: Trail -> Level -> (Trail, [Var])
trailPopToLevel tr targetLv =
    (tr { currentLevel  = targetLv
        , lvBasedTrails = remainingTrails
        , reasons       = foldl' deleteVar (reasons tr) poppedVars
        , levels        = foldl' deleteVar (levels tr) poppedVars
        }
    , poppedVars)
    where
        popSplitIndex                   = currentLevel tr - targetLv
        (poppedTrails, remainingTrails) = splitAt popSplitIndex (lvBasedTrails tr)
        poppedVars                      = concat poppedTrails
        deleteVar intMap var            = IntMap.delete (getVar var) intMap