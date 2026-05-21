module Core.Var where

newtype Var = Var Int deriving (Eq, Ord, Show)

{-# INLINE getVar #-}
getVar :: Var -> Int
getVar (Var i) = i