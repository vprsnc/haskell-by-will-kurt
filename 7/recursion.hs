import GHC.Tc.Utils.TcMType (ExpType(Check))
myGCD a b = if remainder == 0
            then b
            else myGCD b remainder
  where remainder = a `mod` b

sayAmount n = case n of
  1 -> "One"
  2 -> "Two"
  n -> "Many"

myHead (x:xs) = x
myHead [] = error "There is no head for an empty list!"

-- 7.1
myTail (_:xs) = xs
myTail [] = []

-- 7.2
myNewGCD a 0 = a
myNewGCD a b = myNewGCD b (mod a b)
