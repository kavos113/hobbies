import Data.List (sortBy)

names =
  [ ("Bob", 25),
    ("Alice", 30),
    ("Charlie", 20)
  ]

compareAges p1 p2 =
  if age1 > age2
    then GT
    else
      if age1 < age2
        then LT
        else EQ
  where
    age1 = snd p1
    age2 = snd p2

main = do
  let sortedNames = sortBy compareAges names
  print sortedNames