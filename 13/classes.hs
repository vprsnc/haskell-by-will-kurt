class Describable a where
  describe :: a -> String

-- ghci> :i Word
-- ghci> minBound :: Word

-- This will allow printing in ghci
data Icecream = Chocolate | Vanilla deriving (Show, Eq, Ord)

-- 13.3
cycleSucc :: (Bounded a, Enum a, Eq a) => a -> a
cycleSucc n = if n == maxBound
              then minBound
              else succ n
