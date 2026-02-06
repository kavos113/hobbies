greet :: String -> String
greet name = "Hello, " ++ name ++ "!"

main :: IO ()
main = do
  putStrLn "Enter your name:"
  name <- getLine
  let stmt = greet name
  putStrLn stmt