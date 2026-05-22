module Core.Var where

newtype Var = Var Int deriving (Eq, Ord)

{-# INLINE getVar #-}
getVar :: Var -> Int
getVar (Var i) = i