type FirstName = String
type LastName = String
type PatientName = (FirstName, LastName)

type MiddleName = String
data Name = Name FirstName LastName
          | NameWithMiddle FirstName MiddleName LastName deriving (Show)

showName :: Name -> String
showName (Name f l) = f ++ " " ++ l
showName (NameWithMiddle f m l) = f ++ " " ++ m ++ " " ++ l

type Age = Int
type Height = Int
type Weight = Int

data Sex = Male | Female deriving (Show)

data RhType = Pos | Neg deriving (Show)
data ABOType = A | B | AB | O deriving (Show)
data BloodType = BloodType ABOType RhType deriving (Show)

showRh :: RhType -> String
showRh Pos = "+"
showRh Neg = "-"

showABO :: ABOType -> String
showABO A = "A"
showABO B = "B"
showABO AB = "AB"
showABO O = "O"

showBloodType :: BloodType -> String
showBloodType (BloodType abo rh) = showABO abo ++ showRh rh


canDonateTo :: BloodType -> BloodType -> Bool
canDonateTo (BloodType O _) _ = True
canDonateTo _ (BloodType AB _ ) = True
canDonateTo (BloodType A _) (BloodType A _) = True
canDonateTo (BloodType B _) (BloodType B _) = True
canDonateTo _ _ = False

firstName :: PatientName -> String
firstName patient = fst patient

lastName :: PatientName -> String
lastName patient = snd patient


data Patient =  Patient
                { name :: Name
                , sex :: Sex
                , age :: Age
                , height :: Height
                , weight :: Weight
                , bloodType :: BloodType } deriving (Show)

jackieSmith :: Patient
jackieSmith = Patient { name = Name "Jacke" "Smith"
                      , age = 17
                      , sex = Female
                      , height = 157
                      , weight = 48
                      , bloodType = BloodType O Neg }
-- ghci> showName (name JackieSmith)
-- ghci> showBloodType (bloodType JackieSmith)

jackieSmithUpdated = jackieSmith { age = 18 }

-- patientInfo :: PatientName -> Age -> Height -> String
-- patientInfo (fname, lname) age height =
--   name ++ ageHeight
--   where
--     name = lname ++ ", " ++ fname
--     ageHeight = " (Age: " ++ show age ++
--                     "; Height: " ++ show height ++ "cm)"
