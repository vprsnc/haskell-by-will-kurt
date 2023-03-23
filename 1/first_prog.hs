main :: IO ()
main = do
  putStrLn "Who is the recepient?"
  recepient <- getLine
  putStrLn "What is the book name?"
  book <- getLine
  putStrLn "From?"
  name <- getLine
  putStrLn ( createEmail recepient book name )

toPart :: [Char] -> [Char]
toPart recepient = "Dear " ++ recepient ++ "!\n"

bodyPart :: [Char] -> [Char]
bodyPart bookTitle = "Thank you for reading " ++ bookTitle ++ "!\n"

fromPart :: [Char] -> [Char]
fromPart name = "Best regards,\n" ++ name

createEmail :: [Char] -> [Char] -> [Char] -> [Char]
createEmail recepient bookTitle name = toPart recepient ++ bodyPart bookTitle ++ fromPart name
