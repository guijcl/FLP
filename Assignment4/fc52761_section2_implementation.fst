-- 2.1
type Sum = forall t u v . (t -> v) -> (u -> v) -> v

inl : forall t u . t -> (forall v . (t -> v) -> (u -> v) -> v)
inl = \\t => \\u => \l:t -> \\v => \t:(t -> v) -> \u:(u -> v) -> t l

inr : forall t u . u -> (forall v . (t -> v) -> (u -> v) -> v)
inr = \\t => \\u => \r:u -> \\v => \t:(t -> v) -> \u:(u -> v) -> u r

cases : forall t u v . (forall v . (t -> v) -> (u -> v) -> v) -> (t -> v) -> (u -> v) -> v
cases = \\t => \\u => \\v => \val:(forall v . (t -> v) -> (u -> v) -> v) -> 
                             \lc:(t -> v) -> \rc:(u -> v) -> val [v] lc rc


-- 2.2
fromL : forall t u . (forall v . (t -> v) -> (u -> v) -> v) -> t -> t
fromL = \\t => \\u => \l:(forall v . (t -> v) -> (u -> v) -> v) -> \val:t -> 
                       cases [t] [u] [t] l (id [t]) (\rv:u -> val)


-- 2.3
fromR : forall t u . (forall v . (t -> v) -> (u -> v) -> v) -> u -> u
fromR = \\t => \\u => \r:(forall v . (t -> v) -> (u -> v) -> v) -> \val:u -> 
		               cases [t] [u] [u] r (\lv:t -> val) (id [u])


-- 2.4
vl : Bool
-- vl = fromL [Bool] [Int] (inr [Bool] [Int] 4) True //Triggers the Otherwise condition
vl = fromL [Bool] [Int] (inl [Bool] [Int] False) True

-- main : Bool
-- main = vl


-- 2.5
vr : Int
-- vr = fromR [Bool] [Int] (inl [Bool] [Int] False) 2 //Triggers the Otherwise condition
vr = fromR [Bool] [Int] (inr [Bool] [Int] 4) 2

--main : Int
--main = vr