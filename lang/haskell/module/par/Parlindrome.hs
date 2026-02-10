module Parlindrome (isParlindrome) where

import Data.Char (isPunctuation, isSpace, toLower)

stripWs :: String -> String
stripWs = filter (not . isSpace)

stripPunc :: String -> String
stripPunc = filter (not . isPunctuation)

toLowerStr :: String -> String
toLowerStr = map toLower

isParlindrome :: String -> Bool
isParlindrome text = processed == reverse processed
  where
    processed = (stripWs . stripPunc . toLowerStr) text