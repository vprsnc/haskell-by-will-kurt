data SixSideDie = S1 | S2 | S3 | S4 | S5 | S6
  deriving (Enum, Eq, Ord)

instance Show SixSideDie where
  show S1 = "One"
  show S2 = "Two"
  show S3 = "Three"
  show S4 = "Four"
  show S5 = "Five"
  show S6 = "Six"

-- instance Enum SixSideDie where
--   toEnum 0 = S1
--   toEnum 1 = S2
--   toEnum 2 = S3
--   toEnum 3 = S4
--   toEnum 4 = S5
--   toEnum 5 = S6
--   toEnum _ = error "There is no such value"

--   fromEnum S1 = 0
--   fromEnum S2 = 1
--   fromEnum S3 = 2
--   fromEnum S4 = 3
--   fromEnum S5 = 4
--   fromEnum S6 = 5

-- After we realised enum for our die, we can apply the same things
-- as for Int

dieList = [ S1 .. S6 ]

endlessDie = [ S1 .. ]

-- actually generating insatnces automatically most of the cases works
-- better.
-- E.g. endlessDie would give an error from our defined instance.

-- 14.1
data Number = One | Two | Three deriving Enum
instance Eq Number where
  (==) num1 num2 = fromEnum num1 == fromEnum num2

instance Ord Number where
  compare num1 num2 = compare (fromEnum num1) (fromEnum num2)

-- 14.2
data FiveSidedDie = D1 | D2 | D3 | D4 | D5
  deriving (Enum, Eq, Show)

class (Eq a, Enum a) => Die a where
  roll :: Int -> a

instance Die FiveSidedDie where
  roll n = toEnum (n `mod` 5)
