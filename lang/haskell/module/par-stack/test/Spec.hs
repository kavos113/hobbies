import Data.Char (isPunctuation)
import qualified Data.Text as T
import Lib (preprocess)
import Test.QuickCheck
import Test.QuickCheck.Instances

prop_punctuationInvariant text = preprocess text == preprocess noPuncText
  where
    noPuncText = T.filter (not . isPunctuation) text

main :: IO ()
main = do
  quickCheckWith stdArgs { maxSuccess = 1000 } prop_punctuationInvariant
