import Data.Binary (encode)
data FourLetterAlphabet = L1 | L2 | L3 | L4
  deriving (Show, Enum, Bounded)

-- We're adding show, so it's easier to work with it in GHCi
-- Enum to autimatically convert values to Int and use simple maths, also
--      fromEnum and toEnum can are coming handy
-- Bounded to use maxBound and minBound to know how to rotate the alphabet

-- So this how our rotN will work:
-- 1. We'll give it length of the alphabet and the letter to rotate.
-- 2. To find the middle we'll use ~div~, it will show how much we should rotate the letter.
-- 3. If our letter is in the 2nd half of the alphabet, we'll pass
--     the remainer of the dvision of the shift to length of the alphabet.
-- 4. We'll use toEnum to code the Int back to the letter
rotN :: (Bounded a, Enum a) => Int -> a -> a
rotN alphabetLength letter = toEnum rotation
  where alphabetMiddle = alphabetLength `div` 2
        shift = fromEnum letter + alphabetMiddle
        rotation = shift `mod` alphabetLength

-- Now we can easily encode Char, but we still need to know how many
-- chars there are, we can use maxBound for that:
largestChar :: Int
largestChar = fromEnum (maxBound :: Char) -- We're adding Char so that
                                          -- maxBound will be calculated
                                          -- for the right type

-- Since minBound for Char will be zero, length of the alphabet will be n+1

-- We can now encrypt any single Char.
-- To encode a sentence we can create a list of chars.

fourLetterAlphabetEncoder :: [FourLetterAlphabet] ->
                             [FourLetterAlphabet]
fourLetterAlphabetEncoder sentence = map rot4l sentence
  where
    rot4l = rotN alphaSize
    alphaSize = 1 + fromEnum (maxBound :: FourLetterAlphabet)

-- Cool thing about ROT13 model is that if we double encode the message,
-- we will get it's original content

message :: [FourLetterAlphabet]
message = [L1, L3, L4, L1, L1, L2]

encoded = fourLetterAlphabetEncoder message
decoded = fourLetterAlphabetEncoder encoded

-- While this method works well will alphabets of even length,
-- we'll have some problems with decoding alphabet of odd length.
-- Therefore we would need to create function similar to rotN,
-- always adding 1 to the shift, if alphabet lenght is odd.

rotNdecoder :: (Bounded a, Enum a) => Int -> a -> a
rotNdecoder n c = toEnum rotation
  where rotation = shift `mod` n
        shift = if even n
                then fromEnum c + halfN
                else 1 + fromEnum c + halfN
        halfN = n `div` 2

-- Finally we can put it together
rotEncoder :: String -> String
rotEncoder text = map rotChar text
  where rotChar = rotN alphaSize
        alphaSize = 1 + fromEnum (maxBound :: Char)

rotDecoder :: String -> String
rotDecoder text = map rotCharDecoder text
  where rotCharDecoder = rotNdecoder alphaSize
        alphaSize = 1 + fromEnum (maxBound :: Char)
