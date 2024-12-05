type IntStream : 1S = +{Done: End, More: !Int;IntStream}

data List = Nil | Cons Int List

-- Exercise 8
writeIntOnChannel : Int -> Int -> IntStream -> ()
writeIntOnChannel x y c
    | x <= y = writeIntOnChannel (x + 1) y (c |> select More |> send x)
    | otherwise = c |> select Done |> close

-- Exercise 9
writeListOnChannel : List -> IntStream -> ()
writeListOnChannel Nil c = c |> select Done |> close
writeListOnChannel (Cons x xs) c = writeListOnChannel xs (c |> select More |> send x)

-- Exercise 10
readFromChannels : dualof IntStream -> dualof IntStream 1-> IntStream 1-> ()
readFromChannels (Done c) (Done c1) c2 = close c; close c1; c2 |> select Done |> close
readFromChannels (Done c) (More c1) c2 = 
    close c; let (n1, c1) = receive c1 in
    readFromChannels' c1 (c2 |> select More |> send n1)
readFromChannels (More c) (Done c1) c2 = 
    close c1; let (n, c) = receive c in
    readFromChannels' c (c2 |> select More |> send n)
readFromChannels (More c) (More c1) c2 =
    let (n, c) = receive c in
    let (n1, c1) = receive c1 in
    readFromChannels c c1 (c2 |> select More |> send n |> select More |> send n1)

readFromChannels' : dualof IntStream -> IntStream 1-> ()
readFromChannels' (Done c) c1 = close c; c1 |> select Done |> close
readFromChannels' (More c) c1 = 
    let (n, c) = receive c in
    readFromChannels' c (c1 |> select More |> send n)

-- Exercise 11                
readsAndSums : dualof IntStream -> Int
readsAndSums (Done c) = close c; 0
readsAndSums (More c) = let (n, c) = receive c in n + readsAndSums c

-- Exercise 12
main : Int
main =
    let xs = Cons 1 (Cons 2 (Cons 4 (Cons 8 Nil))) in
    let (c, s) = new IntStream in
    let (c1, s1) = new IntStream in
    let (c2, s2) = new IntStream in
    fork @() (\_:() 1-> writeIntOnChannel 1 5 c);
    fork @() (\_:() 1-> writeListOnChannel xs c1);
    fork @() (\_:() 1-> readFromChannels s s1 c2);
    readsAndSums s2