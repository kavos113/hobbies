module Lib
  ( someFunc,
    isParlindrome,
    preprocess,
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

preprocess :: T.Text -> T.Text
preprocess = stripWs . stripPunc . toLowerStr

isParlindrome :: T.Text -> Bool
isParlindrome text = processed == T.reverse processed
  where
    processed = preprocess text