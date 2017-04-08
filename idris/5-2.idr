import Data.Vect

tri : Vect 3 (Double, Double)
tri = [(?tri_rhs1, ?tri_rhs2), (?tri_rhs3, ?tri_rhs4), (?tri_rhs5, ?tri_rhs6)]

four : Vect 4 Nat
four = [0, 0, 0, 0]

Position : Type
Position = (Double, Double)

tri' : Vect 3 Position
tri' = [(?tri'_rhs1, ?tri'_rhs2), (?tri'_rhs3, ?tri'_rhs4), (?tri'_rhs5, ?tri'_rhs6)]

position : Type
position = (Double, Double)

polygon : Nat -> Type
polygon n = Vect n Position

tri'2 : polygon 3

