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

module TypedLambda where

import qualified Data.Set as Set
import qualified Data.Map as Map

-- Names for variables
type Id = String

data TypeId = BBool | Func TypeId TypeId
  deriving
    ( Show
    , Eq, Ord)

data Term =
    TTrue
  | FFalse
  | If Term Term Term
  | Var Id
  | Abs Id TypeId Term
  | App Term Term
  deriving Show

true, false, test :: Term
true = Abs "t" BBool $ Abs "f" BBool $ Var "t"
false = Abs "t" BBool $ Abs "f" BBool $ Var "f"
test = Abs "l" BBool $ Abs "m" BBool $ Abs "n" BBool $ App (App (Var "l") (Var "m")) $ Var "n"

-- Some terms
and, or, not :: Term
and = Abs "b" BBool $ Abs "c" BBool $ App (App (Var "b") (Var "c")) false
or = Abs "b" BBool $ Abs "c" BBool $ App (App (Var "b") true) $ Var "c"
not = Abs "b" BBool $ App (App (Var "b") false) true

boundVars :: Term -> Set.Set (Id, TypeId)
boundVars (Var x) = Set.empty
boundVars (Abs x tp t)
    | x `Set.member`freeVars t = (x, tp) `Set.insert` boundVars t
    |otherwise = boundVars t
boundVars (App t1 t2)  = boundVars t1 `Set.union` boundVars t2


freeVars :: Term -> Set.Set Id
freeVars (Var x)     = Set.singleton x
freeVars (Abs x _ t1)  = freeVars t1 Set.\\ Set.singleton x
freeVars (App t1 t2) = freeVars t1 `Set.union` freeVars t2

-- A fresh variable name, that is, a variable name that is not present in a set
-- of variables.
newId :: Set.Set Id -> Id
newId vs = head [v | n <- [0..], let v = '_':show n, v `Set.notMember` vs]

-- Substitution as in the books of Curry and Feys and Hindley and Seldin. The
-- first and the last equation for Abs are not in TAPL, for TAPL assumes the
-- variable convention, something Haskell cannot.
subs :: Id -> Term -> Term -> Term
subs x s TTrue = TTrue
subs x s FFalse = FFalse
subs x s (If t1 t2 t3) = If (subs x s t1) t2 t3
subs x s (Var y)
  | y == x    = s
  | otherwise = Var y
subs x s (Abs y tp t)
  -- No need to continue in this case
  | x == y = Abs y tp t
  -- The easy case
  | y /= x && (y `Set.notMember` fvS || x `Set.notMember` fvT) = Abs y tp (subs x s t)
  -- Rename if danger of variable capture
  | otherwise = Abs z tp (subs x s (subs y (Var z) t))
  where fvS = freeVars s
        fvT = freeVars t
        z = newId (fvS `Set.union` fvT)
subs x s (App t1 t2) = App (subs x s t1) (subs x s t2)

-- Examples of substitution

-- Hindley and Seldin
t1 :: Term
t1 = subs "x" (Var "w") (Abs "w" BBool (Var "x"))

eval1 :: Term -> Term
eval1 (If TTrue t2 _) = t2
eval1 (If FFalse _ t3) = t3
eval1 (If t1 t2 t3) = If (eval1 t1) t2 t3
eval1 (App (Abs x tp t12) v2) | value v2 = subs x v2 t12
eval1 (App v1 t2) | value v1 = App v1 (eval1 t2)
eval1 (App t1 t2) = App (eval1 t1) t2

eval :: Term -> Term
eval t
  | value t = t
  | otherwise = eval (eval1 t)

value :: Term -> Bool
value TTrue = True
value FFalse = False
value (Abs x tp t1) = True
value _ = False

getType :: TypeId -> TypeId
getType BBool = BBool
getType (Func tp1 tp2) = tp1

getTypeLast :: TypeId -> TypeId
getTypeLast BBool = BBool
getTypeLast (Func tp1 tp2) = tp2


type Context = Map.Map Id TypeId
typeof :: Context -> Term -> TypeId
typeof _ TTrue = BBool
typeof _ FFalse = BBool
typeof ctx (If t1 t2 t3) | typeof ctx t1 == BBool && typeof ctx t2 == typeof ctx t3 = typeof ctx t2
typeof ctx (Var x) = ctx Map.! x
typeof ctx (Abs x tp t1) = Func tp (typeof (Map.insert x tp ctx) t1) 
typeof ctx (App t1 t2) |  typeof ctx t2 == getType (typeof ctx t1) = getTypeLast (typeof ctx t1)

t2, t3:: Term
t2 = App (Abs "x" BBool (Var "x")) TTrue 
t3 = Abs "y" BBool t2