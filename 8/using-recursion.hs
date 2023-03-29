myDrop n [] = []
myDrop n xs =
  reverse ( take nWeNeed ( reverse xs ) )
  where nWeNeed = length xs - n + 1

myDrop' _ [] = []
myDrop' n xs =
  if length xs <= len
  then xs
  else myDrop' n (tail xs)
 where len = length xs - n

myLength [] = 0
myLength (_:xs) = 1 + myLength xs

myTake _ [] = []
myTake 0 _ = []
myTake n (x:xs) =
  let rest = myTake ( n - 1 ) xs
  in x:rest

myCyle [] = error "Imporssible to cycle emtpy list!"
myCycle xs = xs : myCycle xs

myReverse [] = []
myReverse [x] = [x]
myReverse (x:xs) = myReverse xs ++ [x]

fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)


fastFib _ _ 0 = 0
fastFib _ _ 1 = 1
fastFib _ _ 2 = 1
fastFib x y 3 = x + y
fastFib x y c = fastFib (x + y) x (c - 1)

fibTail n = go n (0, 1)
  where
    go 0 (a, b) = a
    go 1 (a, b) = b
    go n (a, b) = go (n - 1) (b, a + b)

fact 0 = 1
fact n = n * fact (n - 1)

map' f []     = []
map' f (x:xs) =  f x  : map' f xs
