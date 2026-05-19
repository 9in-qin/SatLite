module Core.Assignment where

import qualified Data.IntMap as IntMap

import Core.Lit
import Core.Var

type Assignment = IntMap.IntMap Bool
data LitType    = LitTrue | LitFalse | LitUnassigned

literalType :: Assignment -> Lit -> LitType
literalType asgmt lit =
    case IntMap.lookup (getVar (litToVar lit)) asgmt of
        Nothing  -> LitUnassigned
        Just val -> if ifLiteralTrue lit val
                    then LitTrue
                    else LitFalse