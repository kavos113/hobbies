mymap f [] = []
mymap f (x : xs) = (f x) : (mymap f xs)

myfilter test [] = []
myfilter test (x : xs) =
  if test x
    then x : myfilter test xs
    else myfilter test xs

myfilternot test [] = []
myfilternot test (x : xs) =
  if test x
    then myfilternot test xs
    else x : myfilternot test xs

concatAll xs = foldl (++) "" xs

rcons x y = y : x

myreverse xs = foldl rcons [] xs

myfoldl f acc [] = acc
myfoldl f acc (x : xs) = myfoldl f newAcc xs
  where
    newAcc = f acc x

myfoldr f acc [] = acc
myfoldr f acc (x : xs) = f x rest
  where
    rest = myfoldr f acc xs

main = do
  print (mymap (+ 1) [1, 2, 3, 4, 5]) -- Output: [2,3,4,5,6]
  print (mymap (* 2) [1, 2, 3, 4, 5]) -- Output: [2,4,6,8,10]
  print (mymap (\x -> x * x) [1, 2, 3, 4, 5]) -- Output: [1,4,9,16,25]
  print (myfilter even [1, 2, 3, 4, 5, 6]) -- Output: [2,4,6]
  print (myfilter (> 3) [1, 2, 3, 4, 5, 6]) -- Output: [4,5,6]
  print (myfilternot even [1, 2, 3, 4, 5, 6]) -- Output: [1,3,5]
  print (myfilternot (> 3) [1, 2, 3, 4, 5, 6]) -- Output: [1,2,3]
  print (concatAll ["Hello, ", "world", "!"]) -- Output: "Hello, world!"
  print (myreverse [1, 2, 3, 4, 5]) -- Output: [5,4,3,2,1]