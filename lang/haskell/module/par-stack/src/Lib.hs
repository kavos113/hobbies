module Lib
  ( someFunc,
    isParlindrome,
  )
where

import Data.Char (isPunctuation, isSpace, toLower)
import qualified Data.Text as T

someFunc :: IO ()
someFunc = putStrLn "someFunc"

stripWs :: T.Text -> T.Text
stripWs = T.filter (not . isSpace)

stripPunc :: T.Text -> T.Text
stripPunc = T.filter (not . isPunctuation)

toLowerStr :: T.Text -> T.Text
toLowerStr = T.map toLower

isParlindrome :: T.Text -> Bool
isParlindrome text = processed == T.reverse processed
  where
    processed = (stripWs . stripPunc . toLowerStr) text