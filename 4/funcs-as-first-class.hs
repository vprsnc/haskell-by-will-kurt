import Data.List
import GHC.Core.Type (resultIsLevPoly)
import XMonad.Layout.Monitor (Monitor(name))

-- 4.1 Check
ifEven myFunc x = if even x
                  then myFunc x
                  else x


ifEvenCube x = ifEven ( \x -> x ^ 3 ) x

--
names = [("Ian", "Curtis"),
         ("Bernard", "Somner"),
         ("Peter", "Hook"),
         ("Stephen", "Morris")]

--ghci> sort names

compareLastNames name1 name2 = if lastName1 > lastName2
                               then GT
                               else
                                 if lastName1 < lastName2
                                 then LT
                                 else EQ
  where lastName1 = snd name1 -- returns second value of tuple
        lastName2 = snd name2

-- ghci> sortBy compareLastNames names

-- 4.2 check now let's modify it, so that when surnames are EQ, we sort by names

compareLastNamesEq name1 name2 = if lastName1 > lastName2
                               then GT
                               else
                                 if lastName1 < lastName2
                                 then LT
                                 else
                                   if firstName1 > firstName2
                                   then GT
                                   else if firstName1 < firstName2
                                        then LT
                                        else EQ

  where lastName1 = snd name1 -- returns second value of tuple
        lastName2 = snd name2
        firstName1 = fst name1 -- returns first value of tuple
        firstName2 = fst name2

-- Task 4.1: Now we need to rewrite our Compare func with ~compare~, default Haskell function
compareLastNamesC name1 name2 = compare lastName1 lastName2
  where lastName1 = snd name1 -- returns second value of tuple
        lastName2 = snd name2

compareLastNamesCd name1 name2 = if result == EQ
                                 then compare (fst name1) (fst name2)
                                 else result
 where result = compare (snd name1) (snd name2)

-- 4.2 get use of ~case~
-- San-Francisco requires diffrent index for those whos name > L
sfOffce name =
    if lastName < "L"
    then nameText ++
         " - 1254, San-Francisco, California, 94111"
    else nameText ++
         " - 1010, San-Francisco, California, 94109"
  where lastName = snd name
        nameText = fst name ++ " " ++ lastName

-- New York requres ":" after name
nyOffice name = nameText ++
           ": 789, New York, New York State, 10013"
  where nameText = fst name ++ " " ++ snd name

-- Reno office for privacy reasons requires only 2nd name to be present
renoOffice name = nameText ++ " - 456, Reno, Nevada state, 89523"
  where nameText = snd name

-- Now we need to add Washington which requires "Honorable" before the name
washingtonOffice name = "Honoroble " ++ nameText ++
                        " - 1488, Washington, Columbia state, 69420"
  where nameText = fst name ++ " " ++ snd name

-- Now finally our funciton:
getLocationFunc loc =
  case loc of
    "sf" -> sfOffce
    "ny" -> nyOffice
    "reno" -> renoOffice
    "washington" -> washingtonOffice
    _ -> \name -> fst name ++ " " ++ snd name

addressLetter name loc = locationFunction name
  where locationFunction = getLocationFunc loc
