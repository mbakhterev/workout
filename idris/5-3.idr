data Ty = TyNat | TyBool | TyString

%name Ty ty, ty1, ty2, ty3

evalType : Ty -> Type
evalType TyNat = Nat
evalType TyBool = Bool
evalType TyString = String

initVal : (ty : Ty) -> evalType ty
initVal TyNat = 0
initVal TyBool = False
initVal TyString = ""

toString : (ty : Ty) -> evalType ty -> String
toString TyNat x = show x
toString TyBool x = show x
toString TyString x = show x
