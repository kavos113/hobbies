import Control.Monad (replicateM)
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  let linesToRead =
        if not (null args)
          then read (head args)
          else 0 :: Int
  numbers <- replicateM linesToRead getLine
  let ints = map read numbers :: [Int]
  print (sum ints)