module Core.Queue where

import Core.Lit

import qualified Data.Sequence as Seq

type BCPqueue      = Seq.Seq Lit

enqueue :: Lit -> BCPqueue -> BCPqueue
enqueue l q = q Seq.|> l

dequeue :: BCPqueue -> BCPqueue
dequeue q = case Seq.viewl q of
    Seq.EmptyL    -> q
    _ Seq.:< rest -> rest