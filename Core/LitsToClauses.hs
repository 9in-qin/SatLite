module Core.LitsToClauses where

import qualified Data.IntMap as IntMap

import Core.Clause
import Core.Lit

type LitsToClauses = IntMap.IntMap [CID]

updateLitsToClauses :: LitsToClauses -> Int -> CID -> LitsToClauses
-- updateLitsToClauses lsToCls litKey cid =
--     case IntMap.lookup litKey lsToCls of
--         Nothing   -> IntMap.insert litKey [cid] lsToCls
--         Just cids -> IntMap.insert litKey (cid : cids) lsToCls
updateLitsToClauses litsToCls litKey cid =
    IntMap.insertWith (++) litKey [cid] litsToCls