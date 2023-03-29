import Data.Binary.Get (isEmpty)
cup ml = \message -> message ml

coffeeCup = cup 500

getMl aCup = aCup (\ml -> ml)

drink aCup mlDrank = if   mlDiff >= 0
                     then cup mlDiff
                     else cup 0
  where ml      = getMl aCup
        mlDiff  = ml - mlDrank

-- afterSip = drink coffeeCup 30

-- afterTwoSips = drink afterSip 30

-- afterGulp = drink afterTwoSips 120

-- afterBigGulp = drink afterGulp 1000

-- ghci> getMl aftergBigGulp

isEmpty aCup = getMl aCup == 0

afterManySips = foldl drink coffeeCup [30, 30, 30, 30, 30]
