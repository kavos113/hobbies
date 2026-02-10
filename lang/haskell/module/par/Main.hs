module Main where

import qualified Parlindrome

main :: IO ()
main = do
  print "check parlindrome: "
  text <- getLine
  let res = if Parlindrome.isParlindrome text then "yes" else "no"
  print res