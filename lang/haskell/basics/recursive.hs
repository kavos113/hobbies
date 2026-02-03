mygcd a b = if reminder == 0
            then b
            else mygcd b reminder
    where reminder = a `mod` b

mytail (_:xs) = xs

mylength [] = 0
mylength (_:xs) = 1 + mylength xs

mytake 0 _ = []
mytake _ [] = []
mytake n (x:xs) = x:mytake (n-1) xs 

mycycle (first:rest) = first : mycycle (rest ++ [first])

fib n1 n2 0 = n1
fib n1 n2 1 = n2
fib n1 n2 n = fib n2 (n1 + n2) (n - 1)

main = do
    print (mygcd 48 18)  -- Output: 6
    print (mylength [1,2,3,4,5])  -- Output: 5
    print (mytail [1,2,3,4,5])  -- Output: [2,3,4,5]
    print (mytake 3 [1,2,3,4,5])  -- Output: [1,2,3]
    print (take 10 (mycycle [1,2,3]))  -- Output: [1,2,3,1,2,3,1,2,3,1] 
    print (fib 0 1 10)  -- Output: 55