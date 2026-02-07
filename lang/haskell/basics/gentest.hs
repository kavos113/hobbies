testFirstNames :: [String]
testFirstNames = ["Alice", "Bob", "Charlie"]

testLastNames :: [String]
testLastNames = ["Smith", "Johnson", "Williams"]

testScores :: [Int]
testScores = [85, 92, 78]

data User = User
  { firstName :: String,
    lastName :: String,
    score :: Int
  }
  deriving (Show)

generateUsers :: [User]
generateUsers = pure User <*> testFirstNames <*> testLastNames <*> testScores

main :: IO ()
main = print generateUsers