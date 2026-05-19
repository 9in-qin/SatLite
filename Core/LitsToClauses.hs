module Core.LitsToClauses where

import qualified Data.IntMap as IntMap

import Core.Clause

type LitsToClauses = IntMap.IntMap [CID]

updateLitsToClauses :: LitsToClauses -> Int -> CID -> LitsToClauses
updateLitsToClauses litsToCls litKey cid =
    IntMap.insertWith (++) litKey [cid] litsToCls