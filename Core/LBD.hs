module Core.LBD where

import Core.Var
import Core.Lit
import Core.Trail
import Core.Clause

import Data.List

type LBD           = Int

clauseLBD :: Clause -> Trail -> LBD
clauseLBD cl tr = length $ nub [lv | (Var i, bool, lv, reason) <- tr, Lit i `elem` cl]