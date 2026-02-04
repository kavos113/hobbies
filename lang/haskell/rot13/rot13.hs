data FourLetterAlphabet = L1 | L2 | L3 | L4
  deriving (Show, Enum, Bounded)

rotN :: (Bounded a, Enum a) => Int -> a -> a
rotN size c = toEnum rotation
  where
    halfAlphabet = size `div` 2
    offset = fromEnum c + halfAlphabet
    rotation = offset `mod` size

forLetterEncoder :: [FourLetterAlphabet] -> [FourLetterAlphabet]
forLetterEncoder = map rot4
  where
    sizeOfAlphabet = 1 + fromEnum (maxBound :: FourLetterAlphabet)
    rot4 = rotN sizeOfAlphabet

rotChar :: Char -> Char
rotChar = rotN sizeOfAlphabet
  where
    sizeOfAlphabet = 1 + fromEnum (maxBound :: Char)

main = do
  print $ rotN 4 L1 -- should be L3
  print $ rotN 4 L2 -- should be L4
  print $ rotN 4 L3 -- should be L1
  print $ rotN 4 L4 -- should be L2