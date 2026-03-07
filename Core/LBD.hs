module Core.LBD where

import Core.Types
import Data.List

clauseLBD :: Clause -> Trail -> LBD
clauseLBD cl tr = length $ nub [lv | (Var i, bool, lv, reason) <- tr, Lit i `elem` cl]