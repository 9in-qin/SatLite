module Core.Lit where

import Core.Var

import Data.Bits

newtype Lit = Lit Int deriving (Eq, Ord, Show)

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