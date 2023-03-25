doubleDouble x = dubs * 2
  where dubs = x * 2

-- Now let's rewrite it using Lambda func
doubleD x = ( \dubs -> dubs * 2 ) ( x * 2 )


-- Overwriting various funcs with Lambda
sumSquareOrSquareSum x y = let sumSquare = ( x^2 + y^2 )
                               squareSum = ( x + y )^2
                           in
                             if sumSquare > squareSum
                             then sumSquare
                             else squareSum


sumSqOrSqSum x y = ( \sumSquare squareSum ->
                       if sumSquare > squareSum
                       then sumSquare
                       else squareSum ) ( x^2 + y^2 ) (( x + y )^2)


-- Reassigning variable multiple time though it's impossible in Haskell
overwrite x = let x = 2
              in let x = 3
                 in let x = 4
                    in x

ovrw x = (\x ->
           (\x ->
             (\x -> x) 4) 3) 2

-- inc, double, square
inc x = ( \x -> x + 1 )
double x = ( \x -> x * 2 )
square x = ( \x -> x ^ 2 )


-- This will cause an error:
counter x = let x = x + 1
              in
               let x = x + 1
                in
                 x


-- but if we use Lambda:
workingCounter x = ( \x -> x + 1 )
                   (( \x -> x + 1 )
                   (( \x -> x + 1 ) x ))
