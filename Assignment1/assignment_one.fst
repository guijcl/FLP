data Term =
      Zero
    | Succ Term
    | Plus Term Term
    | E
    | A Term
    | B Term
    | Len Term

data Type = Nat | SString

one, two : Term
one = Succ Zero
two = Succ one

s1, s2, l, sl, pl : Term
s1 = Plus one one
s2 = Succ (Plus Zero one)
ln = Len (A (B E))
sl = Succ (Len (B E))
pl = Plus (Len (A E)) two

eval : Term -> Term
eval t
    | value t = t
    | otherwise = eval (eval1 t)

eval1 : Term -> Term
eval1 (Succ t1) = Succ (eval1 t1)
eval1 (Plus Zero t2) | numericValue t2 = t2
eval1 (Plus Zero t2) = Plus Zero (eval1 t2)
eval1 (Plus (Succ t1) t2) = Succ (Plus t1 t2)
eval1 (Plus t1 t2) = Plus (eval1 t1) t2
eval1 (A t1) = t1
eval1 (B t1) = t1
eval1 (Len E) = Zero
eval1 (Len t1) = Succ (Len (eval1 t1))

value : Term -> Bool
value E = True
value t | numericValue t = True
value _ = False

numericValue : Term -> Bool
numericValue Zero = True
numericValue (Succ nv) = numericValue nv
numericValue _ = False

stringValue : Term -> Bool
stringValue E = True
stringValue (A t1) = stringValue t1
stringValue (B t1) = stringValue t1
stringValue _ = False

typeof : Term -> Type
typeof Zero = Nat
typeof (Succ t1) | typeof t == Nat = Nat
typeof (Plus t1 t2) | typeof t1 == Nat && typeof t1 == typeof t2 = Nat
typeof E = SString
typeof (A t1) | typeof t1 == SString = SString
typeof (B t1) | typeof t1 == SString = SString
typeof (Len t1) | typeof t = SString = Nat