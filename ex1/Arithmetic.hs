{-|
Module      : Arithmetic Expressions
Description : Expressions, types, and operations on these, including evaluation and typing

An Haskell adaptation of the code for Types and Programming Languages (TAPL), by
Benjamin Pierce, 2002, The MIT Press.

Fundamentals of Programming Languages
MSc on Informatics Engineering
Faculty of Sciences
University of Lisbon
2022/2023
-}

module Arithmetic where

import qualified Data.Set as Set

-- |Terms
data Term =
-- Figure 3.1, page 34
    TTrue
  | FFalse
  | If Term Term Term
-- Figure 3.2, page 41
  | Zero
  | Succ Term
  | Pred Term
  | IsZero Term
  deriving
    ( Show
    , Eq, Ord) -- So that terms may go into Sets

-- Some terms
s, t, t1, u, v, x :: Term
s = If TTrue FFalse FFalse
t = If s TTrue TTrue
t1 = If s TTrue (Succ TTrue)
u = If FFalse TTrue TTrue
v = If t FFalse FFalse
x = If v v v

-- More terms
one, two, three, cond, err1, err2 :: Term
one = Succ Zero
two = Succ one
three = Succ two
cond = If (IsZero two) two three
err1 = If (IsZero TTrue) one cond
err2 = If (IsZero (Succ TTrue)) one cond

-- |The set of constants appearing in a term, Definition 3.3.1, page 29
consts :: Term -> Set.Set Term
consts TTrue = Set.singleton TTrue
consts FFalse = Set.singleton FFalse
consts Zero = Set.singleton Zero
consts (Succ t) = consts t
consts (Pred t) = consts t
consts (IsZero t) = consts t
consts (If t1 t2 t3) = consts t1 `Set.union` consts t2 `Set.union` consts t3

-- Exercise: size, Definition 3.3.2, page 29
size :: Term -> Int
size TTrue = 1
size FFalse = 1
size Zero = 1
size (Succ t) = size t + 1
size (Pred t) = size t + 1
size (IsZero t) = size t + 1
size (If t1 t2 t3) = size t1 + size t2 + size t3 + 1

-- Exercise: depth, Definition 3.3.2, page 29
depth :: Term -> Int
depth TTrue = 1
depth FFalse = 1
depth Zero = 1
depth (Succ t) = size t + 1
depth (Pred t) = size t + 1
depth (IsZero t) = size t + 1
depth (If t1 t2 t3) = maximum [depth t1, depth t2, depth t3] + 1

-- Evaluation
{- ex 8
  Succ TTrue -> doesn't evaluate
  Succ Zero -> evaluates

  ex 9
  If (If TTrue (Succ Zero) Zero) Zero Zero

  ex 14
  If (If TTrue (TTrue) FFalse) Zero (Succ Zero)

  ex 15
  If (IsZero Zero) TTrue (IsZero FFalse)
-}

-- |Single-step evaluator
eval1 :: Term -> Term
-- Figure 3.1, page 34
eval1 (If TTrue t2 _) = t2
eval1 (If FFalse _ t3) = t3
eval1 (If t1 t2 t3) = If (eval1 t1) t2 t3
-- Figure 3.2, page 41
eval1 (Succ t1) = Succ (eval1 t1)
eval1 (Pred Zero) = Zero
eval1 (Pred (Succ t1)) | numericaValue t1 = t1
eval1 (Pred t1) = Pred (eval1 t1)
eval1 (IsZero Zero) = TTrue
eval1 (IsZero (Succ t1)) | numericaValue t1 = FFalse
eval1 (IsZero t1) = IsZero (eval1 t1)

-- |Numeric value, Figure 3.2, page 41
numericaValue :: Term -> Bool
numericaValue Zero = True
numericaValue (Succ nv) = numericaValue nv
numericaValue _ = False

-- |Value
value :: Term -> Bool
-- Figure 3.1, page 34, values:
value TTrue = True
value FFalse = True
-- Figure 3.2, page 41, values:
value t | numericaValue t = True
value _ = False

-- |Finds the normal form of a given expression, Definition 3.5.9, page 39
eval :: Term -> Term
eval t
  | value t = t
  | otherwise = eval (eval1 t)

-- |Big-step semantics, Exercise 3.5.17
bigstep :: Term -> Term
bigstep t | value t = t
bigstep (If t1 t2 _) | bigstep t1 == TTrue = bigstep t2
bigstep (If t1 _ t3) | bigstep t1 == FFalse = bigstep t3
bigstep (Succ t1) = Succ (bigstep t1)
bigstep (Pred t1) = case bigstep t1 of
  Zero -> Zero
  Succ v | numericaValue v -> v
bigstep (IsZero t) = case bigstep t of
  Zero -> TTrue
  Succ v | numericaValue v -> FFalse

-- |Types, Figure 8.2, page 93  
data Type =
    BBool
  | Nat
  deriving (Show, Eq)

-- |Typing
typeof :: Term -> Type
-- Fig 8.1, page 93
typeof TTrue = BBool
typeof FFalse = BBool
typeof Zero = Nat
-- Fig 8.2, page 93
typeof (If t1 t2 t3) | typeof t1 == BBool && typeof t2 == typeof t3 = typeof t2
typeof (Succ t) | typeof t == Nat = Nat
typeof (Pred t) | typeof t == Nat = Nat
typeof (IsZero t) | typeof t == Nat = BBool

{-

-- Monadic evaluation

import Control.Exception -- For monadic evaluation

data NoRuleApplies = NoRuleApplies deriving Show

instance Exception NoRuleApplies

eval1' :: Term -> IO Term
eval1' (If TTrue t2 _) = pure t2
eval1' (If FFalse _  t3) = pure t3
eval1' (If t1 t2 t3) = If <$> (eval1' t1) <*> (pure t2) <*> (pure t3)
eval1' _ = throw NoRuleApplies
-- eval1' (If t1 t2 t3) = do
--   t1' <- eval1' t1
--   pure $ If t1' t2 t3
-- eval1' (If t1 t2 t3) = liftM3 If (eval1' t1) (pure t2) (pure t3)

eval' :: Term -> IO Term
eval' t = (eval1' t >>= eval') `catch` \(e :: NoRuleApplies) -> pure t

-}
