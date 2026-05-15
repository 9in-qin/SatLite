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

data Trail = Trail { currentLevel  :: Level
                   , lvBasedTrails :: [LvBasedTrail]
                   , reasons       :: Reasons
                   , levels        :: Levels
                   } deriving (Show)

emptyTrail :: Trail
emptyTrail = Trail { currentLevel  = 0
                   , lvBasedTrails = [[]]
                   , reasons       = IntMap.empty
                   , levels        = IntMap.empty
                   }

newLevel :: Trail -> Trail
newLevel tr =
    tr { currentLevel  = currentLevel tr + 1
       , lvBasedTrails = [] : lvBasedTrails tr
       }

trailPush :: Trail -> (Var, Reason) -> Trail
trailPush tr (var, rsn) = -- may need to check if there is a repetitive push
    tr { lvBasedTrails = updatedLvBasedTrails
       , reasons       = IntMap.insert varKey rsn (reasons tr)
       , levels        = IntMap.insert varKey (currentLevel tr) (levels tr)
       }
    where
        varKey = getVar var
        updatedLvBasedTrails =
            case lvBasedTrails tr of
                currentTrail : lowerLvTrails -> (var : currentTrail) : lowerLvTrails
                []                           -> error "Should not."

currentLevelTrail :: Trail -> LvBasedTrail
currentLevelTrail tr =
    case lvBasedTrails tr of
        currentTrail : _ -> currentTrail
        []               -> error "Should not."

trailPopToLevel :: Trail -> Level -> (Trail, [Var])
trailPopToLevel tr targetLv =
    (tr { currentLevel = targetLv
        , lvBasedTrails = remainingTrails
        , reasons       = foldl' deleteVar (reasons tr) poppedVars
        , levels        = foldl' deleteVar (levels tr) poppedVars
        }
    , poppedVars)
    where
        popSplit = currentLevel tr - targetLv
        (poppedTrails, remainingTrails) = splitAt popSplit (lvBasedTrails tr)
        poppedVars = concat poppedTrails
        deleteVar intMap var = IntMap.delete (getVar var) intMap