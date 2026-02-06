{-# LANGUAGE OverloadedStrings #-}

import Data.Text qualified as T
import Data.Text.IO qualified as TIO

greet :: T.Text -> T.Text
greet name = T.concat ["こんにちは、", name, "さん！"]

main = do
  TIO.putStrLn "名前を入力してください："
  name <- TIO.getLine
  let stmt = greet name
  TIO.putStrLn stmt