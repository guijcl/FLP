--g : (Int 1-> Int) -> Int 
--g f = f (f 5)

{-isTen : Int 1-> Bool
isTen x = x == 10
tenOrTwelve : Bool
tenOrTwelve = isTen 10 || isTen 12-}

data List = Nil | Cons (() 1-> ()) List

list : List
list = Cons (\_:()1->()) (Cons (\_:()1->()) (Cons (\_:()1->()) 
       (Cons (\_:()1->()) (Cons (\_:()1->()) Nil))))