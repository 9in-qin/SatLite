module Core.Assignment where

import qualified Data.IntMap as IntMap

import Core.Var
import Core.Lit

type Assignment = IntMap.IntMap Bool

literalType :: Assignment -> Lit -> LitType
literalType asgmt lit =
    case IntMap.lookup (getVar $ litToVar lit) asgmt of
        Nothing  -> LitUnassigned
        Just val -> if (litSign lit && val) || (not (litSign lit) && not val) then LitTrue else LitFalse