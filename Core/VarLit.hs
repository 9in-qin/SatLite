module Core.VarLit where

import Core.Types
import Data.Bits
import qualified Data.Map as Map

getVar :: Var -> Int
getVar (Var i) = i

getLit :: Lit -> Int
getLit (Lit i) = i

varToLit :: Var -> Lit
varToLit (Var i) = Lit (unsafeShiftL i 1)

litToVar :: Lit -> Var
litToVar (Lit i) = Var (unsafeShiftR i 1)

negateLit :: Lit -> Lit
negateLit (Lit i) = Lit (i `xor` 1)

litSign :: Lit -> Bool
litSign (Lit i) = even i

ifLiteralTrue :: Lit -> Bool -> Bool
ifLiteralTrue (Lit i) val = (even i && val) || (odd i && not val)

literalType :: Assignment -> Lit -> LitType
literalType asgmt (Lit i) =
    case Map.lookup (litToVar (Lit i)) asgmt of
        Nothing  -> LitUnassigned
        Just val -> if (even i && val) || (odd i && not val) then LitTrue else LitFalse