module Engine.DPLL where

import Control.Applicative
import Data.Maybe
import qualified Data.Map as Map
import qualified Data.IntMap as IntMap

import Core.Types
import Core.VarLit
import Decide.Arbitrary

dpll :: ClauseDB -> SolverState -> Result'
dpll db ss = fromMaybe UNSAT'
    (propagate (clauses db) (varCount db) (assignment ss))

propagate :: Clauses -> VarCount -> Assignment -> Maybe Result'
propagate cls totalVar asgmt =
    case unAssigned totalVar asgmt of
        Just (Var nextVar) ->
            let setTrue  = assignVar cls asgmt (Var nextVar) True >>= propagate cls totalVar
                setFalse = assignVar cls asgmt (Var nextVar) False >>= propagate cls totalVar
            in setTrue <|> setFalse
        Nothing -> Just (SAT' asgmt)

assignVar :: Clauses -> Assignment -> Var -> Bool -> Maybe Assignment
assignVar cls asgmt (Var i) val =
    let asgmt' = IntMap.insert i val asgmt
    in if leadsToFalseClause cls asgmt' (Var i)
            then Nothing
            else Just asgmt'

leadsToFalseClause :: Clauses -> Assignment -> Var -> Bool
leadsToFalseClause cls asgmt (Var i) =
    let listCls = Map.toList cls
        cls' = [(cid, lits)| (cid, lits) <- listCls, Lit i' <- lits, abs i' == i]
    in any (allFalse asgmt) cls'
    where
        allFalse :: Assignment -> (CID, Clause) -> Bool
        allFalse asgmt (cid, cl) = not $ any (nothingOrTrue . getLitValue asgmt) cl

getLitValue :: Assignment -> Lit -> Maybe Bool
getLitValue asgmt (Lit i)
    | i > 0     = result
    | otherwise = fmap not result
    where result = IntMap.lookup (abs i) asgmt

nothingOrTrue :: Maybe Bool -> Bool
nothingOrTrue Nothing = True
nothingOrTrue (Just val) = val