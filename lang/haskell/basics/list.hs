assign n aList = zip groups aList
  where
    groups = cycle [1 .. n]

main = do
  let assigned = assign 3 ["apple", "banana", "cherry", "date", "elderberry"]
  print assigned