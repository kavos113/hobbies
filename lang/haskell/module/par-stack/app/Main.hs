module Main (main) where

import Data.Text as T
import Data.Text.IO as TIO
import Lib

main :: IO ()
main = do
  TIO.putStrLn "check parlindrome"
  text <- TIO.getLine
  let res = if isParlindrome text then "yes" else "no"
  TIO.putStrLn res
