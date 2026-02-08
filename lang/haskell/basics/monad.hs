echo :: IO ()
-- getLine :: IO String
-- putStrLn :: String -> IO ()
-- (>>=) :: IO a -> (a -> IO b) -> IO b
echo = getLine >>= putStrLn

askName :: IO ()
askName = putStrLn "What is your name?"

createGreeting :: String -> String
createGreeting name = "Hello, " ++ name ++ "!"

greet :: IO ()
greet = askName >> getLine >>= (\name -> return (createGreeting name)) >>= putStrLn

pows :: Int -> [Int]
pows n = do
  x <- [1 .. n]
  return (2 ^ x)

-- no syntax sugar version
pows' :: Int -> [Int]
pows' n = [1 .. n] >>= (\x -> return (2 ^ x))

-- list comprehension version
pows'' :: Int -> [Int]
pows'' n = [2 ^ x | x <- [1 .. n]]