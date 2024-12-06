-- 3.1
type List = forall x r . (x -> r -> r) -> r -> r


-- 3.2
nil : forall x . (forall r . (x -> r -> r) -> r -> r)
nil = \\x => \\r => \c:(x -> r -> r) -> \n:r -> n

cons : forall x . x -> (forall r . (x -> r -> r) -> r -> r) -> (forall r . (x -> r -> r) -> r -> r)
cons = \\x => \hd:x -> \tl:(forall r . (x -> r -> r) -> r -> r) -> 
       \\r => \c:(x -> r -> r) -> \n:r -> c hd (tl [r] c n)


-- 3.3
null : forall x . (forall r . (x -> r -> r) -> r -> r) -> Bool
null = \\x => \l:(forall r . (x -> r -> r) -> r -> r) -> l [Bool] (\hd:x -> \tl:Bool -> False) True


-- 3.4
length : forall x . (forall r . (x -> r -> r) -> r -> r) -> Int
length = \\x => \l:(forall r . (x -> r -> r) -> r -> r) -> l [Int] (\i:x -> \z:Int -> z + 1) 0


-- 3.5
head : forall x . (forall r . (x -> r -> r) -> r -> r) -> x
head = \\x => \l:(forall r . (x -> r -> r) -> r -> r) -> 
		      (l [() -> x] (\hd:x -> \tl:(() -> x) -> \_:() -> hd) (diverge [x])) ()

diverge : forall x . () -> x
diverge = \\x => \e:() -> diverge [x] e


-- 3.6
type Pair = forall x y r . (x -> y -> r) -> r

pair : forall x y . x -> y -> (forall r . (x -> y -> r) -> r)
pair = \\x => \\y => \e1:x -> \e2:y -> \\r => \f:(x -> y -> r) -> f e1 e2

fst' : forall x y . (forall r . (x -> y -> r) -> r) -> x
fst' = \\x => \\y => \p:(forall r . (x -> y -> r) -> r) -> p [x] (\ex:x -> \ey:y -> ex)

snd' : forall x y . (forall r . (x -> y -> r) -> r) -> y
snd' = \\x => \\y => \p:(forall r . (x -> y -> r) -> r) -> p [y] (\ex:x -> \ey:y -> ey)

tail : forall x . (forall r . (x -> r -> r) -> r -> r) -> (forall r . (x -> r -> r) -> r -> r)
tail = \\x => \l:(forall r . (x -> r -> r) -> r -> r) -> 
		(fst' [forall r . (x -> r -> r) -> r -> r] [forall r . (x -> r -> r) -> r -> r] (
          l [forall r . ((forall r . (x -> r -> r) -> r -> r) -> (forall r . (x -> r -> r) -> r -> r) -> r) -> r]
          	(\hd:x -> \tl:(forall r . ((forall r . (x -> r -> r) -> r -> r) 
            -> (forall r . (x -> r -> r) -> r -> r) -> r) -> r) ->
              pair [forall r . (x -> r -> r) -> r -> r] [forall r . (x -> r -> r) -> r -> r]
              (snd' [forall r . (x -> r -> r) -> r -> r] [forall r . (x -> r -> r) -> r -> r] tl)
              (cons [x] hd (snd' [forall r . (x -> r -> r) -> r -> r] 
              [forall r . (x -> r -> r) -> r -> r] tl))) (pair 
                [forall r . (x -> r -> r) -> r -> r] [forall r . (x -> r -> r) -> r -> r]
          		(nil [x]) (nil [x]))))


-- 3.7
type Nat = forall x . (x -> x) -> x -> x

replicate : forall x . Nat -> x -> (forall r . (x -> r -> r) -> r -> r)
replicate = \\x => \n:Nat -> \v:x -> 
			        n [forall r . (x -> r -> r) -> r -> r] (cons [x] v) (nil [x])


-- 3.8
four : Nat
four s z = s (s (s (s z)))

main : Int
main = length [Char] $ replicate [Char] four 'a'