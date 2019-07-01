{-# LANGUAGE RankNTypes #-}

import Data.Void

type AxiomPEM = forall a. forall b. (a -> b) -> ((a -> Void) -> b) -> b
type AxiomDNE = forall a. ((a -> Void) -> Void) -> a

from :: AxiomDNE -> AxiomPEM
from dne = undefined

to :: AxiomPEM -> AxiomDNE
to pem = pem (flip absurd)
