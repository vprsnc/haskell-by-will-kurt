-- This is a simple program to calcualte how much change
-- we should give against how much money we were given
calcChange owed given = if change > 0
                        then change
                        else 0
  where change = given - owed

-- 1. Why it is not possible to use ~if~ without ~else~?
-- Becaues Function has always to return a Value!

-- 2.
inc n howMuch = n + howMuch

double n = n * 2

square n = n ** 2

foo n = if even n
        then n - 2
        else 3 * n + 1
