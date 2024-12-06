data Term =
      Zero
    | Succ Term
    | Sum Term Term
    | E
    | A Term
    | B Term
    | Len Term
    deriving (Eq, Show, Ord)

data Type = Nat | SString
    deriving (Eq, Show, Ord)

s1, s2, ln, sl, pl :: Term
s1 = Sum (Succ Zero) (Succ Zero)
s2 = Succ (Sum Zero (Succ Zero))
ln = Len (A (B E))
sl = Succ (Len (B E))
pl = Sum (Len (A E)) (Succ (Succ Zero))

eval :: Term -> Term
eval t
    | value t   = t
    | otherwise = eval (eval1 t)

eval1 :: Term -> Term
eval1 (Succ t1) = Succ (eval1 t1)
eval1 (Sum Zero t2) | numericValue t2 = t2
eval1 (Sum Zero t2) = Sum Zero (eval1 t2)
eval1 (Sum (Succ t1) t2) = Succ (Sum t1 t2)
eval1 (Sum t1 t2) = Sum (eval1 t1) t2
eval1 (A t1) | stringValue t1 = t1
eval1 (B t1) | stringValue t1 = t1
eval1 (Len E)  = Zero
eval1 (Len t1) = Succ (Len (eval1 t1))

value :: Term -> Bool
value t1 | stringValue t1  = True
value t1 | numericValue t1 = True
value _                    = False

numericValue :: Term -> Bool
numericValue Zero      = True
numericValue (Succ nv) = numericValue nv
numericValue _         = False

stringValue :: Term -> Bool
stringValue E      = True
stringValue (A t1) = stringValue t1
stringValue (B t1) = stringValue t1
stringValue _      = False

typeof :: Term -> Type
typeof Zero = Nat
typeof (Succ t) | typeof t == Nat = Nat
typeof (Sum t1 t2) | typeof t1 == Nat  && typeof t1 == typeof t2 = Nat
typeof E = SString
typeof (A t) | typeof t == SString = SString
typeof (B t) | typeof t == SString = SString
typeof (Len t) | typeof t == SString = Nat