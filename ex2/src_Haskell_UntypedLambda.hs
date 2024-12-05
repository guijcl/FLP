{-|
Module      : The untyped lambda-calculus
Description : Terms, substitution, evaluation.

An Haskell adaptation of the code for Types and Programming Languages (TAPL), by
Benjamin Pierce, The MIT Press (2002), except for substitution which is taken
from Hindley, J.R., Seldin, J.P.: Introduction to Combinators and
Lambda-Calculus, Cambridge University Press (1986)

Fundamentals of Programming Languages
MSc on Informatics Engineering
Faculty of Sciences
University of Lisbon
2022/23
-}

module UntypedLambda where

import qualified Data.Set as Set

-- Names for variables
type Id = String 

data Term =
    Var Id
  | Abs Id Term
  | App Term Term
  deriving Show

-- Some terms

-- true, false, test, and :: Term
-- TODO
true, false, test, and, or, not :: Term
true = Abs "t" $ Abs "f" $ Var "t" 
false = Abs "t" $ Abs "f" $ Var "f"
test = Abs "l" $ Abs "m" $ Abs "n" $ App (App (Var "l") (Var "m")) $ Var "n"
and = Abs "b" $ Abs "c" $ App (App (Var "b") (Var "c")) false
or = Abs "b" $ Abs "c" $ App (App (Var "b") true) $ Var "c"
not = Abs "b" $ App (App (Var "b") false) true

zero, one, two, plus, times :: Term
zero = Abs "s" $ Abs "z" $ Var "z"
one = Abs "s" $ Abs "z" $ App (Var "s") $ Var "z"
two = Abs "s" $ Abs "z" $ App (Var "s") (App (Var "s") (Var "z"))
plus = Abs "m" $ Abs "n" $ Abs "s" $ Abs "z" $ App (App (Var "m") (Var "s")) 
                (App (App (Var "s") (Var "z")) (Var "n"))
times = Abs "m" $ Abs "n" $ App (Var "m") (App (App plus (Var "n")) zero) 

size :: Term -> Int
size (Var x) = 1
size (Abs x t1) = 1 + size t1
size (App t1 t2) = size t1 + size t2

depth :: Term -> Int
depth (Var x) = 1
depth (Abs x t1) = 1 + depth t1
depth (App t1 t2) = depth t1 `max` depth t2

eval1 :: Term -> Term
eval1 (App (Abs x t12) v2) | value v2 = subs x v2 t12
eval1 (App v1 t2) | value v1 = App v1 (eval1 t2)
eval1 (App t1 t2) = App (eval1 t1) t2

eval :: Term -> Term
eval t
  | value t = t
  | otherwise = eval (eval1 t)

value :: Term -> Bool
value (Abs x t1) = True
value _ = False

-- The set of the free variables of an expression
freeVars :: Term -> Set.Set Id
freeVars (Var x)     = Set.singleton x
freeVars (Abs x t1)  = freeVars t1 Set.\\ Set.singleton x
freeVars (App t1 t2) = freeVars t1 `Set.union` freeVars t2

-- A fresh variable name, that is, a variable name that is not present in a set
-- of variables.
newId :: Set.Set Id -> Id
newId vs = head [v | n <- [0..], let v = '_':show n, v `Set.notMember` vs]

-- Substitution as in the books of Curry and Feys and Hindley and Seldin. The
-- first and the last equation for Abs are not in TAPL, for TAPL assumes the
-- variable convention, something Haskell cannot.
subs :: Id -> Term -> Term -> Term
subs x s (Var y)
  | y == x    = s
  | otherwise = Var y
subs x s (Abs y t)
  -- No need to continue in this case
  | x == y = Abs y t
  -- The easy case
  | y /= x && (y `Set.notMember` fvS || x `Set.notMember` fvT) = Abs y (subs x s t)
  -- Rename if danger of variable capture
  | otherwise = Abs z (subs x s (subs y (Var z) t))
  where fvS = freeVars s
        fvT = freeVars t
        z = newId (fvS `Set.union` fvT)
subs x s (App t1 t2) = App (subs x s t1) (subs x s t2)

-- Examples of substitution

-- Hindley and Seldin
t1, t2, t3, t4, t5, t6, t7, t8 :: Term
t1 = subs "x" (Var "w") (Abs "w" (Var "x"))
t2 = subs "x" (App (Var "u") (Var "v")) (Abs "y" (App (Var "x") (Abs "w" (App (Var "v") (App (Var "w") (Var "x"))))))
t3 = subs "x" (Abs "y" (App (Var "v") (Var "y"))) (App (Var "y") (Abs "v" (App (Var "x") (Var "v"))))
t4 = subs "x" (Abs "_0" (App (Var "_1") (Var "_0"))) (App (Var "_0") (Abs "_1" (App (Var "x") (Var "_1"))))
-- TAPL, page 70
t5 = subs "x" (Var "y") (Abs "x" (Var "x"))
t6 = subs "_0" (Var "_1") (Abs "_0" (Var "_0"))
-- Others
t7 = subs "x" (Var "z") (Abs "z" (Var "x"))
t8 = subs "_0" (Var "_1") (Abs "_1" (Var "_0"))
