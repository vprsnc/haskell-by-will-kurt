-- 6.1 we need to define ~repeat~ function

myRepeat n = cycle [n]

-- 6.2 create subseq function returning slice of a list
subseq start end n =
  case n of
    [] -> []
    _ -> drop start (take end n)

-- 6.3  create ~inFirstHalf~ returning True if element in the 1st half of the list
inFirstHalf myList n = n `elem` firstHalf
 where
   firstHalf = take half myList
   half = div (length myList) 2
