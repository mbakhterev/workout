{-# LANGUAGE RankNTypes #-}

import Data.Void

type AxiomPEM = forall a. forall b. (a -> b) -> ((a -> Void) -> b) -> b
type AxiomDNE = forall a. ((a -> Void) -> Void) -> a

xfn :: (a -> b) -> ((a -> Void) -> b) -> (a, a -> Void) -> b
xfn f g p = seq (f (fst p)) (g (snd p))

from :: AxiomDNE -> AxiomPEM
from dne ab avb = dne (\xv -> xv (avb (xv . ab)))

h :: a -> ((a -> Void) -> Void) -> a
h x y = x

g :: (a -> Void) -> ((a -> Void) -> Void) -> a
g x y = absurd (y x)

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

r :: (A -> Void) -> Void
r = undefined

s :: ((A -> Void) -> Void) -> Void
s = undefined

u :: A -> Void
u = undefined
