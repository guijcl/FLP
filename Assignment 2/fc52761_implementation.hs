import qualified Data.Set as Set
import qualified Data.Map as Map

type Id = String

type Context = Map.Map Id Type

data Type = Nat | BBool | Arrow Type Type
  deriving
    (Show, Eq, Ord)

data Term =
    Zero
  | TTrue
  | FFalse
  | Succ Term
  | If Term Term Term
  | Var Id
  | Abs Id Type Term
  | App Term Term
  | Rec Term Id Id Term Term 
  deriving Show

eval :: Term -> Term
eval t
  | value t = t
  | otherwise = eval (eval1 t)

eval1 :: Term -> Term
eval1 Zero = Zero
eval1 (If TTrue t2 _) = t2
eval1 (If FFalse _ t3) = t3
eval1 (Succ t) = Succ (eval1 t)
eval1 (If t1 t2 t3) = If (eval1 t1) t2 t3
eval1 (App (Abs x ty t12) v2) | value v2 = subs x v2 t12
eval1 (App v1 t2) | value v1 = App v1 (eval1 t2)
eval1 (App t1 t2) = App (eval1 t1) t2
eval1 (Rec t0 x y t1 Zero) = t0 
eval1 (Rec t0 x y t1 (Succ nv)) | numericalValue nv = subs x nv (subs y (Rec t0 x y t1 nv) t1)
eval1 (Rec t0 x y t1 t) = Rec t0 x y t1 (eval1 t)

subs :: Id -> Term -> Term -> Term
subs x s Zero = Zero
subs x s TTrue = TTrue
subs x s FFalse = FFalse
subs x s (Succ t) = Succ (subs x s t)
subs x s (If t1 t2 t3) = If (subs x s t1) t2 t3
subs x s (Var y)
  | y == x    = s
  | otherwise = Var y
subs x s (Abs y ty t)
  | x == y = Abs y ty t
  | y /= x && (y `Set.notMember` fvS || x `Set.notMember` fvT) = Abs y ty (subs x s t)
  | otherwise = Abs z ty (subs x s (subs y (Var z) t))
  where fvS = freeVars s
        fvT = freeVars t
        z = newId (fvS `Set.union` fvT)
subs x s (App t1 t2) = App (subs x s t1) (subs x s t2)
subs x s (Rec t0 y z t1 t) = Rec (subs x s t0) y z (subs x s t1) (subs x s t)

freeVars :: Term -> Set.Set Id
freeVars Zero = Set.empty
freeVars (Succ t) = freeVars t
freeVars (Var x) = Set.singleton x
freeVars (Abs x _ t1) = freeVars t1 Set.\\ Set.singleton x
freeVars (App t1 t2) = freeVars t1 `Set.union` freeVars t2
freeVars (Rec t0 x y t1 t) = freeVars t0 `Set.union` freeVars t `Set.union` 
                             freeVars (Abs x Nat (Abs y (typeof Map.empty (Rec t0 x y t1 t)) t1))

typeof :: Context -> Term -> Type
typeof _ Zero = Nat
typeof _ TTrue = BBool
typeof _ FFalse = BBool
typeof ctx (Succ t) | typeof ctx t == Nat = Nat
typeof ctx (If t1 t2 t3) | typeof ctx t1 == BBool && typeof ctx t2 == typeof ctx t3 = typeof ctx t2
typeof ctx (Var x) = ctx Map.! x
typeof ctx (Abs x ty t1) = Arrow ty (typeof (Map.insert x ty ctx) t1) 
typeof ctx (App t1 t2) | t11 == typeof ctx t2 = t12 
  where Arrow t11 t12 = typeof ctx t1
typeof ctx (Rec t0 x y t1 t) 
  | ty0 == typeof (Map.insert y ty0 (Map.insert x Nat ctx)) t1 && typeof ctx t == Nat = ty0
  where ty0 = typeof ctx t0


value :: Term -> Bool
value t | numericalValue t = True
value TTrue = True
value FFalse = False
value (Abs {}) = True
value _ = False

numericalValue :: Term -> Bool
numericalValue Zero = True
numericalValue (Succ nv) = numericalValue nv
numericalValue _ = False

newId :: Set.Set Id -> Id
newId vs = head [v | n <- [0..], let v = '_':show n, v `Set.notMember` vs]