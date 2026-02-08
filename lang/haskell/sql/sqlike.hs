import Control.Monad (guard)

data Name = Name
  { firstName :: String,
    lastName :: String
  }

instance Show Name where
  show (Name f l) = mconcat [f, " ", l]

data GradeLevel
  = Freshman
  | Sophomore
  | Junior
  | Senior
  deriving (Eq, Ord, Enum, Show)

data Student = Student
  { studentId :: Int,
    studentName :: Name,
    gradeLevel :: GradeLevel
  }
  deriving (Show)

students :: [Student]
students =
  [ Student 1 (Name "Tanaka" "Taro") Senior,
    Student 2 (Name "Yamada" "Hanako") Junior,
    Student 3 (Name "Sato" "Ichiro") Freshman,
    Student 4 (Name "Suzuki" "Jiro") Sophomore
  ]

-- a -> b: property access function
_select :: (a -> b) -> [a] -> [b]
_select f list = do
  x <- list
  return (f x)

_where :: (a -> Bool) -> [a] -> [a]
_where f list = do
  x <- list
  guard (f x)
  return x

startsWith :: Char -> String -> Bool
startsWith c string = c == head string

data Teacher = Teacher
  { teacherId :: Int,
    teacherName :: Name
  }
  deriving (Show)

teachers :: [Teacher]
teachers =
  [ Teacher 100 (Name "Kato" "Masaru"),
    Teacher 200 (Name "Tanaka" "Yuki")
  ]

data Course = Course
  { courseId :: Int,
    courseTitle :: String,
    teacher :: Int
  }
  deriving (Show)

courses :: [Course]
courses =
  [ Course 101 "Functional Programming" 100,
    Course 201 "Introduction to Programming" 200,
    Course 301 "Algorithms and Data Structures" 100,
    Course 401 "Database Systems" 200,
    Course 501 "Operating Systems" 100
  ]

-- a -> c, b -> c: property access functions
_join :: (Eq c) => [a] -> [b] -> (a -> c) -> (b -> c) -> [(a, b)]
_join listA listB f g = do
  x <- listA
  y <- listB
  let pair = (x, y)
  guard (f (fst pair) == g (snd pair))
  return pair

main :: IO ()
main = do
  print $ _select studentName students
  print $ _select gradeLevel students
  print $ _select (firstName . studentName) students
  print $ _where (startsWith 'S' . firstName) (_select studentName students)
  print $ _join teachers courses teacherId teacher