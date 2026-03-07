module Core.Queue where

import Core.Types

import qualified Data.Sequence as Seq

enqueue :: Lit -> BCPqueue -> BCPqueue
enqueue l q = q Seq.|> l

dequeue :: BCPqueue -> BCPqueue
dequeue q = case Seq.viewl q of
    Seq.EmptyL    -> q
    _ Seq.:< rest -> rest