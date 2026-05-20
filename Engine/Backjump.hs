module Engine.Backjump where

import qualified Data.IntMap as IntMap
import qualified Data.Sequence as Seq
import Data.List

import Core.Var
import Core.Lit
import Core.Clause
import Core.Queue
import Core.Trail
import Core.ClauseDB
import Core.SolverState

backjump :: ClauseDB -> SolverState -> Clause -> Var -> Level -> (ClauseDB, SolverState)
backjump db ss learnedCl firstUIP currentLevel =
    (db { clauses     = IntMap.insert newCID learnedCl (clauses db)
        , clauseCount = clauseCount db + 1
        },
     ss { assignment = IntMap.insert (getVar firstUIP) (litSign firstUIPLit) asgmtWithoutPoppedVars
        , queue      = enqueue firstUIPLit Seq.empty
        , trail      = trailPush trailAfterPop (firstUIP, Propagated newCID)
        })
    where
        newCID                 = clauseCount db
        asgmt                  = assignment ss
        tr                     = trail ss
        levelInfo              = levels tr
        backjumpLevel          = case learnedCl of
                                        [_] -> 0
                                        _   -> foldl' levelChecker 0 learnedCl
        (trailAfterPop,
         poppedVars)           = trailPopToLevel tr backjumpLevel
        asgmtWithoutPoppedVars = foldl' (\acc x -> IntMap.delete (getVar x) acc) asgmt poppedVars
        firstUIPLit            = head $ filter (\lit -> litToVar lit == firstUIP) learnedCl

        levelChecker :: Level -> Lit -> Level
        levelChecker lv lit =
            let litLv = levelInfo IntMap.! getVar (litToVar lit)
            in if litLv > lv && litLv /= currentLevel
               then litLv
               else lv