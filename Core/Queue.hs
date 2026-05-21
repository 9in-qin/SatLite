module Core.Queue where

import Core.Lit

import qualified Data.Sequence as Seq

type BCPqueue = Seq.Seq Lit

emptyQueue :: BCPqueue
emptyQueue = Seq.empty

enqueue :: Lit -> BCPqueue -> BCPqueue
enqueue l q =
    q Seq.|> l

dequeue :: BCPqueue -> BCPqueue
dequeue q =
    case Seq.viewl q of
        Seq.EmptyL    -> q
        _ Seq.:< rest -> rest

enqueueUnitClauses :: [Lit] -> BCPqueue -> BCPqueue
enqueueUnitClauses lits q = q Seq.>< Seq.fromList lits