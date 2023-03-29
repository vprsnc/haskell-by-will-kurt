import GHC.Unicode (toLower)
import Graphics.X11.Xrandr (xrrConfigCurrentConfiguration)

map' f [] = []
map' f (x:xs) = f x : map' f xs

filter' test [] = []
filter' test (x:xs) = if test x
                      then x:filter' test xs
                      else filter' test xs

-- check 9.1 we need to create ~remove~ func
remove test [] = []
remove test (x:xs) =
  if test x
  then remove test xs
  else x:remove test xs

-- check 9.2 we need to create ~product~ func for all elems in list
product' xs = foldl (*) 1 xs

sumOfSquares xs = foldl (+) 0 (map (^2) xs)

foldl' f init [] = init
foldl' f init (x:xs) = foldl' f newInit xs
  where newInit = f init x

foldr' f init [] = init
foldr' f init (x:xs) = f x rightResult
  where rightResult = foldr' f init xs

-- 9.1 redefine ~elem~ func with the help of ~filter~ and ~length~
elem' y [] = False
elem' y (x:xs) =
  if y == x
  then True
  else elem' y xs

elem'' y xs = let lambda = filter ( == y ) xs
                  in length lambda > 0

-- 9.2 write isPalyndrome function ignoring the case and whitespaces
-- A man, a plan, a canal – Panama
isPalyndrome' t = processedText == reverse processedText
  where noSpaces = filter (/= ' ') t
        processedText = map toLower noSpaces

-- 9.3 write a lazy ~harmonic~ function
harmonic a = sum (take a seriesVals)
  where seriesPairs = zip (cycle [1.0]) [ 1.0, 2.0 .. ]
        seriesVals = map (\pair -> fst pair / snd pair)
                          seriesPairs
