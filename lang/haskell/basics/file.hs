import System.IO

main :: IO ()
main = do
  f <- openFile "hello.txt" ReadMode

  firstLine <- hGetLine f
  putStrLn firstLine

  secondLine <- hGetLine f
  anotherFile <- openFile "another.txt" WriteMode
  hPutStrLn anotherFile secondLine
  hClose anotherFile

  hClose f