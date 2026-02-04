xorBool :: Bool -> Bool -> Bool
xorBool a b = (a || b) && not (a && b)

xorPair :: (Bool, Bool) -> Bool
xorPair (a, b) = xorBool a b

xor :: [Bool] -> [Bool] -> [Bool]
xor xs ys = map xorPair (zip xs ys)

type Bits = [Bool]

intToBits' :: Int -> Bits
intToBits' 0 = [False]
intToBits' 1 = [True]
intToBits' n =
  if remainder == 0
    then False : intToBits' nextVal
    else True : intToBits' nextVal
  where
    remainder = n `mod` 2
    nextVal = n `div` 2

maxBits :: Int
maxBits = length (intToBits' maxBound)

intToBits :: Int -> Bits
intToBits n = paddings ++ bits
  where
    bits = reverse (intToBits' n)
    missings = maxBits - length bits
    paddings = take missings (cycle [False])

charToBits :: Char -> Bits
charToBits c = intToBits (fromEnum c)

bitsToInt :: Bits -> Int
bitsToInt bits = sum (map (\x -> 2 ^ snd x) trueLocations)
  where
    size = length bits
    indices = [size - 1, size - 2 .. 0]
    trueLocations = filter (\x -> fst x == True) (zip bits indices)

bitsToChar :: Bits -> Char
bitsToChar bits = toEnum (bitsToInt bits)

mypad :: String
mypad = "cipher"

myplaintext :: String
myplaintext = "attackatdawn"

applyotp' :: String -> String -> [Bits]
applyotp' pad plaintext = map (\pair -> (fst pair) `xor` (snd pair)) zippedBits
  where
    padBits = map charToBits (cycle pad)
    plaintextBits = map charToBits plaintext
    zippedBits = zip padBits plaintextBits

applyotp :: String -> String -> String
applyotp pad plaintext = map bitsToChar (applyotp' pad plaintext)

class Cipher a where
  encode :: a -> String -> String
  decode :: a -> String -> String

data OneTimePad = OTP String

instance Cipher OneTimePad where
  encode (OTP pad) plaintext = applyotp pad plaintext
  decode (OTP pad) ciphertext = applyotp pad ciphertext

myotp :: OneTimePad
myotp = OTP (cycle [minBound .. maxBound])

main = do
  print (bitsToChar (charToBits 'a')) -- should be 'a'
  print (encode myotp myplaintext) -- should be ciphertext
  print (decode myotp (encode myotp myplaintext)) -- should be "attackatdawn"