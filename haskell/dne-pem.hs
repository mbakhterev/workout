{-# LANGUAGE RankNTypes #-}

import Data.Void

type AxiomPEM = forall a. forall b. (a -> b) -> ((a -> Void) -> b) -> b
type AxiomDNE = forall a. ((a -> Void) -> Void) -> a

h :: a -> ((a -> Void) -> Void) -> a
h x y = x

g :: (a -> Void) -> ((a -> Void) -> Void) -> a
g x y = absurd (y x)


from :: AxiomDNE -> AxiomPEM
from dne = undefined -- \f g -> f id

to :: AxiomPEM -> AxiomDNE
to pem = pem h g

dne :: AxiomDNE
dne = undefined

pem :: AxiomPEM
pem = undefined

data A = A
data B = B

p :: A -> B
p = undefined

q :: (A -> Void) -> B
q = undefined

-- r :: AxiomDNE -> (((a -> Void) -> Void) -> Void) -> Void
-- r dne f = f dne
