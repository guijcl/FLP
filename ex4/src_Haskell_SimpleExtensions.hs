import qualified Data.Set as Set
import qualified Data.Map as Map

type Id = String

data Type = 
    Nat 
  | BBool 
  | Arrow Type Type 
  | RecType (Map.Map Id Type)
  deriving (Show, Eq, Ord)

data Pattern =
    VarP Id
  | RecP (Map.Map Id Pattern)
  deriving (Show, Eq, Ord)

--data Binding = IdBind Id | PatternBind Pattern
--  deriving (Show, Eq, Ord)

type Context = Map.Map Id Type

data Term =
    Zero
  | TTrue
  | FFalse
  | Pred Term
  | Succ Term
  | IsZero Term
  | If Term Term Term
  | Var Id
  | Abs Id Type Term
  | App Term Term
  | Let Id Term Term
  | LetP Pattern Term Term
  | Rec (Map.Map Id Term)
  | RecProj Term Id
  | Fix Term
  deriving (Show, Eq)
        
plus, times :: Term -> Term -> Term
plus m = App (App (Fix (Abs "p" (Arrow Nat (Arrow Nat Nat)) 
            (Abs "m" Nat 
              (Abs "n" Nat 
                (If (IsZero (Var "m")) (Var "n") (Succ (App (App (Var "p") (Pred (Var "m"))) (Var "n")))))))) m)
times m = App (App (Fix (Abs "t" (Arrow Nat (Arrow Nat Nat)) 
            (Abs "m" Nat 
              (Abs "n" Nat 
                (If (IsZero (Var "m")) Zero (plus (Var "n") (App (App (Var "t") (Pred (Var "m"))) (Var "n")))))))) m)

factorial :: Term -> Term
factorial = App (Fix (Abs "f" (Arrow Nat (Arrow Nat Nat)) 
            (Abs "m" Nat 
                (If (IsZero (Var "m")) (Succ Zero) (times (Var "m") (App (Var "f") (Pred (Var "m"))))))))

eval :: Term -> Term
eval t
  | value t = t
  | otherwise = eval (eval1 t)

eval1 :: Term -> Term
eval1 Zero = Zero
eval1 (If TTrue t2 _) = t2
eval1 (If FFalse _ t3) = t3
eval1 (Succ t) = Succ (eval1 t)
eval1 (Pred Zero) = Zero
eval1 (Pred (Succ t1)) | numericalValue t1 = t1
eval1 (Pred t1) = Pred (eval1 t1)
eval1 (IsZero Zero) = TTrue
eval1 (IsZero (Succ t1)) | numericalValue t1 = FFalse
eval1 (IsZero t1) = IsZero (eval1 t1)
eval1 (If t1 t2 t3) = If (eval1 t1) t2 t3
eval1 (App (Abs x ty t12) v2) | value v2 = subs x v2 t12
eval1 (App v1 t2) | value v1 = App v1 (eval1 t2)
eval1 (App t1 t2) = App (eval1 t1) t2
eval1 (Let x v1 t2) | value v1 = subs x v1 t2
eval1 (Let x t1 t2) = Let x (eval1 t1) t2
eval1 (LetP p v1 t2) | value v1 = match p v1 t2
eval1 (LetP p t1 t2) = LetP p (eval1 t1) t2
eval1 (RecProj (Rec m) x) = m Map.! x
eval1 (RecProj t x) = RecProj (eval1 t) x
eval1 (Rec m) = Rec (Map.map (\x -> if value x then x else eval1 x) m)
eval1 (Fix (Abs x t1 t2)) = subs x (Fix (Abs x t1 t2)) t2
eval1 (Fix t) = Fix (eval1 t)

match :: Pattern -> Term -> (Term -> Term)
match (VarP x) v | value v = subs x v
match (RecP m1) (Rec m2) | value (Rec m2) && (length m1 == length m2) && (Map.keysSet m1 == Map.keysSet m2) =
  foldl (.) id (zipWith match (map snd (Map.toList m1)) (map snd (Map.toList m2)))

subs :: Id -> Term -> Term -> Term
subs x s Zero = Zero
subs x s TTrue = TTrue
subs x s FFalse = FFalse
subs x s (Succ t) = Succ (subs x s t)
subs x s (Pred t) = Pred (subs x s t)
subs x s (IsZero t) = IsZero (subs x s t)
subs x s (If t1 t2 t3) = If (subs x s t1) (subs x s t2) (subs x s t3)
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
subs x s (Let y t1 t2) = Let y (subs x s t1) (subs x s t2)
subs x s (LetP p t1 t2) = LetP p (subs x s t1) (subs x s t2)
subs x s (RecProj t y) = RecProj (subs x s t) y
subs x s (Rec m) = Rec (Map.map (subs x s) m)
subs x s (Fix t) = Fix (subs x s t)

freeVars :: Term -> Set.Set Id
freeVars Zero = Set.empty
freeVars TTrue = Set.empty
freeVars FFalse = Set.empty
freeVars (Succ t) = freeVars t
freeVars (Pred t) = freeVars t
freeVars (IsZero t) = freeVars t
freeVars (If t1 t2 t3) =  freeVars t1 `Set.union` freeVars t2 `Set.union` freeVars t3
freeVars (Var x) = Set.singleton x
freeVars (Abs x _ t1) = freeVars t1 Set.\\ Set.singleton x
freeVars (App t1 t2) = freeVars t1 `Set.union` freeVars t2
freeVars (Let y t1 t2) = freeVars t1 `Set.union` (freeVars t2 Set.\\ Set.singleton y)
freeVars (LetP p t1 t2) = freeVars t1 `Set.union` (freeVars t2 Set.\\ freeVarsP p)
freeVars (RecProj t x) = freeVars t
freeVars (Rec m) = foldl (\x y -> x `Set.union` freeVars y) Set.empty m
freeVars (Fix t) = freeVars t

freeVarsP :: Pattern -> Set.Set Id
freeVarsP (VarP x) = Set.singleton x
freeVarsP (RecP m) = 
  foldl (\x (y1, y2) -> x `Set.union` freeVarsP y2) Set.empty (Map.toList m)

typeof :: Context -> Term -> Type
typeof _ Zero = Nat
typeof _ TTrue = BBool
typeof _ FFalse = BBool
typeof ctx (Succ t) | typeof ctx t == Nat = Nat
typeof ctx (Pred t) | typeof ctx t == Nat = Nat
typeof ctx (IsZero t) | typeof ctx t == Nat = BBool
typeof ctx (If t1 t2 t3) 
  | typeof ctx t1 == BBool && typeof ctx t2 == typeof ctx t3 = typeof ctx t2
typeof ctx (Var x) = ctx Map.! x
typeof ctx (Abs x ty t1) = Arrow ty (typeof (Map.insert x ty ctx) t1) 
typeof ctx (App t1 t2) | t11 == typeof ctx t2 = t12 
  where Arrow t11 t12 = typeof ctx t1
typeof ctx (Let x t1 t2) = typeof (Map.insert x (typeof ctx t1) ctx) t2
typeof ctx (LetP p t1 t2) = typeof (Map.union ctx (delta p ty1)) t2
  where ty1 = typeof ctx t1
typeof ctx (Rec m) = RecType (Map.map (typeof ctx) m)
typeof ctx (RecProj t x) = m Map.! x
  where (RecType m) = typeof ctx t
typeof ctx (Fix t) | t11 == t12 = t12
  where Arrow t11 t12 = typeof ctx t

delta :: Pattern -> Type -> Context
delta (VarP x) ty = Map.insert x ty Map.empty
delta (RecP m1) (RecType m2) = 
  foldMap (uncurry delta) (zip (map snd (Map.toList m1)) (map snd (Map.toList m2)))

value :: Term -> Bool
value t | numericalValue t = True
value TTrue = True
value FFalse = True
value (Abs {}) = True
value (Rec m) = foldl (\x y -> x && value y) True m
value _ = False

numericalValue :: Term -> Bool
numericalValue Zero = True
numericalValue (Succ nv) = numericalValue nv
numericalValue _ = False

newId :: Set.Set Id -> Id
newId vs = head [v | n <- [0..], let v = '_':show n, v `Set.notMember` vs]



-- ===================================================================================================
-- 1.A

{- The final rule:
for each i, p_i:X_i    for each i, j, i /= j => X_i \cap X_j = 0
----------------------------------------------------------------
            pattern : (X_1 Union ... Union X_n)
-}
pwrcd :: Pattern -> Set.Set Id
pwrcd (VarP x) = Set.singleton x
pwrcd (RecP m) | premise m = foldl (\x y -> x `Set.union` freeVarsP y) Set.empty m

-- The premise summarized
premise :: Map.Map Id Pattern -> Bool
premise m = foldl (\x y -> x && pairwise y (Map.toList m)) True m

-- for each i we check if X_i is pairwise distinct in X_{i..n}
-- (this could've been done much better, probably)
pairwise :: Pattern -> [(Id, Pattern)] -> Bool
pairwise p [] = True
pairwise p [x] = True
pairwise (VarP i) (_:x:xs) = i `notElem` getVars x && pairwise (VarP i) (x:xs)
pairwise (RecP m) (_:x:xs) = 
      foldl (\y z -> y && innerRec (getVars z) (getVars x)) True (Map.toList m)
      && pairwise (RecP m) xs
  where innerRec l1 l2 = foldl (\y2 z2 -> y2 && (z2 `elem` l2)) True l1

-- Get a X (Vars in Pattern)
getVars :: (Id, Pattern) -> [Id]
getVars (x, VarP y) = [y]
getVars (x, RecP p) = foldl (\y y2 -> y ++ getVars y2) [] (Map.toList p)