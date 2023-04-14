-- XOR is logical exclusive-or,  means that (T, T) -> F
-- So, if we have two lists of chars, we can encode the first one;
-- Second list should be random though.
-- First of all, we'll need to create our xorbool class since
-- Bool in Haskell does not contain XOR
xorBool :: Bool -> Bool -> Bool
xorBool x y = ( x || y ) &&
              not ( x && y )

-- xor cryptomethod works well if we have other text than our message
-- of the same length.
-- So, we'll need xorPair.
xorPair :: (Bool, Bool) -> Bool
xorPair (x, y) = xorBool x y

-- Finally, we need to put it altoghether,
-- so that xor func will work on two lists of chars
xor :: [Bool] -> [Bool] -> [Bool]
xor xs ys = map xorPair ( zip xs ys )

-- The only problem is that we want to encode the actual message,
-- and not list of Bools :D
-- So we need to cast string to bits.

type Bits = [Bool]

-- We know that each character is basically an integer in the table of chars (utf-8).
-- Therefore we can cast integer to bits.

intToBits' :: Int -> Bits
intToBits' 0 = [False]
intToBits' 1 = [True]
intToBits' n = if remainder == 0
               then False : intToBits' nextN
               else True  : intToBits' nextN
        where remainder = n `mod` 2
              nextN     = n `div` 2

-- This works well except for that our final number is reversed!
-- And also we want all the lists be of the same length.
-- E.g. [0] will return [False], while intToBits' maxBound, well,
-- they should be of the same lenght by inserting Falses.
maxBits :: Int
maxBits = length (intToBits' maxBound)

intToBits :: Int -> Bits
intToBits  n = leadingFalses ++ reversedBits
  where leadingFalses = take missingBits (cycle [False])
        missingBits   = maxBits - length reversedBits
        reversedBits  = reverse (intToBits' n)

-- Finally, we can translate signle Char to bit
charToBits :: Char -> Bits
charToBits c = intToBits (fromEnum c)

-- What we need also, is a way to translate back
bitsToInt :: Bits -> Int
bitsToInt bits = sum ( map ( \x -> 2^(snd x) ) trueLocations )
  where trueLocations = filter ( \x -> fst x == True )
                          ( zip bits indices )
        indices = [ size -1, size -2 .. 0 ]
        size = length bits

bitsToChar :: Bits -> Char
bitsToChar bits = toEnum ( bitsToInt bits )

-- All we need now is OTP!
-- True random one time pad makes XOR almost impossible to break.
applyOTP' :: String -> String -> [Bits]
applyOTP' otp s =
  map ( \pair -> (fst pair) `xor` (snd pair) )
      ( zip otpBits sBits )
 where otpBits = map charToBits otp
       sBits = map charToBits s

-- Obviously applyOTP' returns only bits, while we need String,
-- so the final applyOTP will look like that:
applyOTP :: String -> String -> String
applyOTP otp s= map bitsToChar bitList
  where bitList = applyOTP' otp s

-- Let's give a quick test
myString :: String
myString = "Haskell"

myOTP :: String
myOTP = "Shhhhhh"

--ghci> applyOTP myOTP myString

-- Interesting fact: we can decode our message just like we encoded it.
-- Using partial application we can create encoder/decoder

encoderDecoder :: String -> String
encoderDecoder = applyOTP myOTP

-- ghci> encoderDecoder "book"
-- ghci> encoderDecoder "1\a\a\ETX"

-- With one time pad, we know have a much better algorythm to encrypt our messages.
-- Importtant thing is that OTP should be random enough, and should be used
-- only one time!

-- Let's now put together all we have done to a common class:
class Cipher a where
  encode :: a -> String -> String
  decode :: a -> String -> String

-- Now we need a data type for one time pad, and make it a specimen of our class.

newtype OneTimePad = OTP String

instance Cipher OneTimePad where
  encode (OTP otp) text = applyOTP otp text
  decode (OTP otp) text = applyOTP otp text

-- Now, if our message is no longer than the One TIme pad, everything should be fine.
-- But how can we make it possible to encode any message?

myOTP' :: OneTimePad
myOTP' = OTP (cycle [ minBound .. maxBound ])

encoderDecoder' :: String -> String
encoderDecoder' = encode myOTP'


-- The taks: use pseudoradom number generator to create truely
-- random OTP
-- Using PRNG for encrypting is called stream ciphering
prng :: Int -> Int -> Int -> Int -> Int
prng a b maxNumber seed = ( a*seed + b ) `mod` maxNumber

testPRNG :: Int -> Int
testPRNG = prng 1337 7 100

--ghci> examplePRNG 12345

-- Now we need to create an instance of the Cipher class
--instance Cipher StreamCipher where
