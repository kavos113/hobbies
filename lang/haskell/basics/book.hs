type FirstName = String

type LastName = String

type MiddleName = String

data Name
  = Name FirstName LastName
  | NameWithMiddle FirstName MiddleName LastName
  deriving (Show)

data Author
  = Author Name
  | AuthorGroup [Name]
  deriving (Show)

data Book = Book
  { author :: Author,
    title :: String,
    year :: Int,
    bookPrice :: Double
  }
  deriving (Show)

data Magazine = Magazine
  { editor :: Name,
    magTitle :: String,
    issueNumber :: Int,
    magPrice :: Double
  }
  deriving (Show)

data Pamphlet = Pamphlet
  { pamphletTitle :: String,
    pamphletPrice :: Double
  }
  deriving (Show)

data Publication
  = BookPublication Book
  | MagazinePublication Magazine
  | PamphletPublication Pamphlet
  deriving (Show)

abook :: Publication
abook =
  BookPublication
    ( Book
        { author = Author (Name "George" "Orwell"),
          title = "1984",
          year = 1949,
          bookPrice = 9.99
        }
    )

main = do
  print abook