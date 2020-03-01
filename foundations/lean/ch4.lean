constant test : Π (x y : ℕ), (x + y) = (y + x)
#check test

variables (m n : ℕ)

#check add_mul n m n
#check eq.trans
#check mul_add (m + n) m n
#check add_assoc

example (x : ℕ) : ℕ → ℕ := (λ y : ℕ, x + y)

example (x y: ℕ) : (x + y) * (x + y) = x * x + y * x + x * y + y * y :=
  (λ h1 : (x + y) * (x + y) = (x + y) * x + (x + y) * y,
    (λ h2 : (x + y) * (x + y) = x * x + x * y + (x * y + y * y), _
    )
    (eq.subst (eq.subst (add_mul x y y) h1))
    )
  (mul_add (x + y) x y)

universe u
variable P : ℕ → Prop
variables (x y : ℕ)
variable h1 : (x + y) * (x + y) = (x + y) * x + (x + y) * y

#check (λ h1 : (x + y) * (x + y) = (x + y) * x + (x + y) * y, h1) (mul_add (x + y) x y)

#check @eq.subst

/- example (x y : ℕ) : (x + y) * (x + y) = x * x + y * x + x * y + y * y :=
(λ h1 : (x + y) * (x + y) = (x + y) * x + (x + y) * y,
  (λ h2 : (x + y) * (x + y) = x * x + x * y + (x * y + y * y),
      h2.trans (add_assoc (x * x + y * x) (x * y) (y * y)).symm))
    eq.subst (add_mul x y x) (eq.subst (add_mul x y y) h1)
(mul_add (x + y) x y) -/

#check add_mul x y y
constant h3 : (x + y) * x + x * y + y * y :=  eq.subst (add_mul x y y) h1

#check Π (a:Prop), a -> Prop

constant h4 : Π (a : ℕ), (x + y) * x + a = (x + y) * x + a
#check h4

def pair_eq ∀ (a b c d : Type), a = b → c = d → (a, c) = (b, d) := refl

#check add_mul x y x
#check add_mul x y y
#check mul_add (x + y) x y
#check refl 
#check @eq.trans
#check @eq.subst
#check @eq.refl
#check eq.refl (x + y)

variables (a b : ℕ)
constant eqab : a = b
#check eqab

#check eq.refl a
#check eq.subst (eqab 1 1) 

#check (1:ℕ)
#check @eq.refl ℕ 

#check @eq.subst
#check @eq.subst ℕ (λ n:ℕ, n = n) ((x + y) * x) (x * x + y * x) (add_mul x y x) (eq.refl ((x + y) * x))
-- #check @eq.subst u (eq.refl a) (λ m, eq.refl (m + a))

def add_same (n m : ℕ) : n + m = n + m := eq.refl (n + m)
def same_add (n m : ℕ) : m + n = m + n := eq.refl (m + n)

#check add_same x
#check same_add x

#check (λ n:ℕ, add_same n)

#check eq.trans (mul_add (x + y) x y) (ppeq (add_mul x y x) (add_mul x y y))

example (x y : ℕ) : (x + y) * (x + y) = x * x + y * x + x * y + y * y :=
  have h1 : (x + y) * (x + y) = (x + y) * x + (x + y) * y,
    from mul_add (x + y) x y,
  have h2 : (x + y) * (x + y) = x * x + y * x + (x * y + y * y),
    from eq.subst (add_mul x y x) (eq.subst (add_mul x y y) h1),
  h2.trans (add_assoc (x * x + y * x) (x * y) (y * y)).symm

-- Доказательство того, что a + b = c + d, если a = c и b = d. Отсюда имя ppeq - plus plus equal.
def ppeq : ∀ {a b c d : ℕ}, a = c → b = d → a + b = c + d :=
 (λ a b c d eqac eqbd,
   (eq.subst eqac (eq.subst eqbd (eq.refl (a + b)))))

#check ppeq (add_mul x y x) (add_mul x y y)
#check mul_add (x + y) x y

def test_1 (x y : ℕ) : (x + y) * (x + y) = x * x + y * x + x * y + y * y :=
    (eq.trans (eq.trans (mul_add (x + y) x y) (ppeq (add_mul x y x) (add_mul x y y)))
              (eq.symm (add_assoc (x * x + y * x) (x * y) (y * y))))

#check test_1

variables (p q r : ℕ)
variable (epq : p = q)

#check ()  
#check @eq.subst ℕ (λ i, i+r = i+r) p q epq (eq.refl (p + r)) -- становится несколько понятнее
