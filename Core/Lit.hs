module Core.Lit where

import Data.Bits

import Core.Var

newtype Lit = Lit Int deriving (Eq, Ord)

{-# INLINE getLit #-}
getLit :: Lit -> Int
getLit (Lit i) = i

{-# INLINE varToLit #-}
varToLit :: Var -> Lit
varToLit (Var i) = Lit (unsafeShiftL i 1)

{-# INLINE litToVar #-}
litToVar :: Lit -> Var
litToVar (Lit i) = Var (unsafeShiftR i 1)

negateLit :: Lit -> Lit
negateLit (Lit i) = Lit (i `xor` 1)

{-# INLINE litSign #-}
litSign :: Lit -> Bool
litSign (Lit i) = even i

{-# INLINE ifLiteralTrue #-}
ifLiteralTrue :: Lit -> Bool -> Bool
ifLiteralTrue (Lit i) val = (even i && val) || (odd i && not val)